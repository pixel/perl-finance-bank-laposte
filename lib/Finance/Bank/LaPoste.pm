package Finance::Bank::LaPoste;

use strict;

use Carp qw(carp croak);
use HTTP::Cookies;
use LWP::UserAgent;
use HTML::Parser;
use HTML::Form;
use Digest::MD5();

our $VERSION = '7.00';

# $Id: $
# $Log: LaPoste.pm,v $

=pod

=head1 NAME

Finance::Bank::LaPoste -  Check your "La Poste" accounts from Perl

=head1 SYNOPSIS

 use Finance::Bank::LaPoste;

 my @accounts = Finance::Bank::LaPoste->check_balance(
    username => "0120123456L",	# your main account is something like 
				# 0123456 L 012, stick it together with the region first
    password => "123456",	# a password is usually 6 numbers
 );

 foreach my $account (@accounts) {
    print "Name: ", $account->name, " Account_no: ", $account->account_no, "\n", "*" x 80, "\n";
    print $_->as_string, "\n" foreach $account->statements;
 }

=head1 DESCRIPTION

This module provides a read-only interface to the Videoposte online banking
system at L<https://www.videoposte.com/>. You will need either Crypt::SSLeay
installed.

The interface of this module is similar to other Finance::Bank::* modules.

=head1 WARNING

This is code for B<online banking>, and that means B<your money>, and that
means B<BE CAREFUL>. You are encouraged, nay, expected, to audit the source
of this module yourself to reassure yourself that I am not doing anything
untoward with your banking data. This software is useful to me, but is
provided under B<NO GUARANTEE>, explicit or implied.

=cut

my $parse_table = sub {
    my ($html) = @_;
    my $h = HTML::Parser->new;

    my (@l, $row, $td, $href);
    $h->report_tags('td', 'tr', 'a');
    $h->handler(start => sub { 
        my ($tag, $attr) = @_;
        if ($tag eq 'tr') {
	    $row = [];
        } elsif ($tag eq 'td') {
	    push @$row, ('') x ($attr->{colspan} - 1) if $attr->{colspan};
	    $td = '';
        } elsif ($tag eq 'a') {
	    $href = $attr->{href} if defined $td;
        }
    }, 'tag,attr');
    $h->handler(end => sub { 
        my ($tag) = @_;
        if ($tag eq '/tr') {
	    push @l, $row if $row;
	    undef $row;
        } elsif ($tag eq '/td' && defined $td) {
	    $td =~ s/(|&nbsp;|\s)+/ /g;
	    $td =~ s/^\s*//;
	    $td =~ s/\s*$//;
	    push @$row, $href ? [ $td, $href ] : $td;
	    $href = $td = undef;
        }
    }, 'tag');
    
    $h->handler(text => sub { 
        my ($text) = @_;
        $td .= " $text" if defined $td;
    }, 'text');
    $h->parse($html);

    \@l;
};

my $normalize_number = sub {
    my ($s) = @_;
    $s =~ s/\xC2?\xA0//; # non breakable space, both in UTF8 and latin1
    $s =~ s/ //;
    $s =~ s/,/./;
    $s + 0; # turn into a number
};

=pod

=head1 METHODS

=head2 new(username => "0120123456L", password => "123456", feedback => sub { warn "Finance::Bank::LaPoste: $_[0]\n" })

Return an object . You can optionally provide to this method a LWP::UserAgent
object (argument named "ua"). You can also provide a function used for
feedback (useful for verbose mode or debugging) (argument named "feedback")

=cut

my $first_url = 'https://voscomptesenligne.labanquepostale.fr/voscomptes/canalXHTML/identif.ea?origin=particuliers';
my $base_url = 'https://voscomptesenligne.labanquepostale.fr/voscomptes/canalXHTML';

sub _login {
    my ($self) = @_;
    $self->{feedback}->("login") if $self->{feedback};

    my $cookie_jar = HTTP::Cookies->new;
    my $response = $self->{ua}->request(HTTP::Request->new(GET => $first_url));
    $cookie_jar->extract_cookies($response);
    $self->{ua}->cookie_jar($cookie_jar);

    my %mangling_map = _get_number_mangling_map($self);
    my $password = join('', map { $mangling_map{$_} } split('', $self->{password}));

    my $form = HTML::Form->parse($response->content, $first_url);
    $form->value(username => $self->{username});
    $form->value(password => $password);

    push @{$self->{ua}->requests_redirectable}, 'POST';
    $response = $self->{ua}->request($form->click);
    $response->is_success or die "login failed\n" . $response->error_as_HTML;
}

sub _output { my $f = shift; open(my $F, ">$f") or die "output in file $f failed: $!\n"; print $F $_ foreach @_; 1 }

# to update %img_md5sum_to_number, set $debug_imgs to 1, 
# then rename /tmp/[a-j].gif into /tmp/[0-9].gif according to the image
# then do "md5sum /tmp/[0-9].gif"
my $debug_imgs = 0;
my %img_md5sum_to_number = (
    'a02574d7bf67677d2a86b7bfc5e864fe' => 0,
    'eb85e1cc45dd6bdb3cab65c002d7ac8a' => 1,
    '596e6fbd54d5b111fe5df8a4948e80a4' => 2,
    '9cdc989a4310554e7f5484d0d27a86ce' => 3,
    '0183943de6c0e331f3b9fc49c704ac6d' => 4,
    '291b9987225193ab1347301b241e2187' => 5,
    '163279f1a46082408613d12394e4042a' => 6,
    'b0a9c740c4cada01eb691b4acda4daea' => 7,
    '3c4307ee92a1f3b571a3c542eafcb330' => 8,
    'c5b854ae314b61ba42948909e0b2eae7' => 9,
);

sub _get_number_mangling_map {
    my ($self) = @_;

    my $i = 0;
    map { 
	my $md5sum = Digest::MD5::md5_hex($_);
	my $number = $img_md5sum_to_number{$md5sum};
	defined $number or die "unknown md5sum $md5sum, please update \%img_md5sum_to_number\n";
	$number => $i++;
    } _get_imgs($self);
}

sub _get_imgs {
    my ($self) = @_;

    my @imgs = map {
	my $url = 'https://voscomptesenligne.labanquepostale.fr/wsost/OstBrokerWeb/loginform?imgid=' . $_ . '&' . rand();
	_GET_content($self, $url);
    } 0 .. 9;

    if ($debug_imgs) {
	my $i = 'a';
	_output("/tmp/" . $i++ . ".gif", $_) foreach @imgs;
    }
    @imgs;
}

sub _GET_content {
    my ($self, $url) = @_;

    my $req = $self->{ua}->request(HTTP::Request->new(GET => $url));
    $req->is_success or die "getting $url failed\n" . $req->error_as_HTML;
    $req->content;
}

sub _list_accounts {
    my ($self) = @_;
    $self->{feedback}->("list accounts") if $self->{feedback};
    my $response = $self->{ua}->request(HTTP::Request->new(GET => "$base_url/releve/syntheseAssurancesEtComptes.ea"));
    $response->is_success or die "can't access account\n" . $response->error_as_HTML;

    if ($response->content =~ /frame src=".*liste_comptes.jsp"|<a href=".*aiguillagePersonnalisation.ea"/) {
	$response = $self->{ua}->request(HTTP::Request->new(GET => "$base_url/releve/liste_comptes.jsp"));
	$response->is_success or die "can't access account\n" . $response->error_as_HTML;
    }

    my $accounts = $parse_table->($response->content);
    map {
	my ($account, $account_no, $balance) = grep { $_ ne '' } @$_;
	if (ref $account && $account_no) {
	    my $url = $account->[1];
	    $url =~ s/typeRecherche=1$/typeRecherche=10/; # 400 last operations
	    $url =~ s!/relevesCCP/\d-!/relevesCCP/!; # remove the unneeded number (otherwise one get an intermediate page)
	    {
	        name => $account->[0],
	        account_no => $account_no, 
	        balance => $normalize_number->($balance),
		$url =~ /(releve_ccp|releve_cne|releve_cb|mouvementsCarteDD)\.ea/ ? (url => $url) : (), 
	    };
	} else { () }
    } @$accounts;
}

sub new {
    my ($class, %opts) = @_;
    my $self = bless \%opts, $class;

    exists $self->{password} or croak "Must provide a password";
    exists $self->{username} or croak "Must provide a username";

    $self->{ua} ||= LWP::UserAgent->new;

    _login($self);
    $self;
}

sub default_account {
    die "default_account can't be used anymore";
}

=pod

=head2 check_balance(username => "0120123456L", password => "123456")

Return a list of account (F::B::LaPoste::Account) objects, one for each of
your bank accounts.

=cut

sub check_balance {
    my $self = &new;

    $self->{accounts} = [ _list_accounts($self) ];

    map { Finance::Bank::LaPoste::Account->new($self, %$_) } @{$self->{accounts}};
}

package Finance::Bank::LaPoste::Account;

=pod

=head1 Account methods

=head2 sort_code()

Return the sort code of the account. Currently, it returns an undefined
value.

=head2 name()

Returns the human-readable name of the account.

=head2 account_no()

Return the account number, in the form C<0123456L012>.

=head2 balance()

Returns the balance of the account.

=head2 statements()

Return a list of Statement object (Finance::Bank::LaPoste::Statement).

=head2 currency()

Returns the currency of the account as a three letter ISO code (EUR, CHF,
etc.).

=cut

sub new {
    my ($class, $bank, %account) = @_;
    $account{$_} = $bank->{$_} foreach qw(ua feedback);
    bless \%account, $class;
}

sub sort_code  { undef }
sub name       { $_[0]{name} }
sub account_no { $_[0]{account_no} }
sub balance    { $_[0]{balance} }
sub currency   { 'EUR' }
sub statements { 
    my ($self) = @_;
    $self->{url} or return;
    $self->{statements} ||= do {
    my $retry;
      retry:
	$self->{feedback}->("get statements") if $self->{feedback};
	my $response = $self->{ua}->request(HTTP::Request->new(GET => "$base_url/releve/$self->{url}"));
	$response->is_success or die "can't access account $self->{name} statements\n" . $response->error_as_HTML;

	my $html = $response->content;

	if ($html =~ /D&eacute;tail de vos cartes/ && !$retry) {
	    my @l = $html =~ /a href="(3-mouvementsCarteDD.ea.*?)"/g;
	    $self->{url} = "../relevesCB/" . $l[-1]; # taking last (??)
	    $retry++;
	    goto retry;
	}

	my ($solde_month, $year) = 
	  $html =~ /Solde\s+au\s+\d+\s+(\S+)\s+(20\d\d)/ ? ($1, $2) :
	  $html =~ m!au \d\d/(\d\d)/(20\d\d)!;

	$self->{balance} ||= do {
	    my ($balance) = $html =~ /(?:Solde|Encours\s+pr&eacute;lev&eacute;)\s+au.*?:\s+(.*?)\beuros/s;
	    $balance =~ s/<.*?>\s*//g; # (since 24/06/2004) remove: </span><span class="soldeur"> or <strong>...</strong>
	    $normalize_number->($balance);
	};
	my $l = $parse_table->($html);

	    # drop first line which contain columns title
	    @$l = map {
		my ($date, $description, $amount) = @$_;
		$date && $date =~ m!(\d+)/(\d+)! ? [ $date, $description, $amount ] : ();
	    } @$l;

	my $prev_month = $solde_month eq 'janvier' || $solde_month eq '01' ? 1 : 12;
	[ map {
	    my ($date, $description, $amount) = @$_;
	    my ($day, $month) = $date =~ m|(\d+)/(\d+)|;
	    $year-- if $month > $prev_month;
	    $prev_month = $month;
	    Finance::Bank::LaPoste::Statement->new(day => $day, month => $month, year => $year, description => $description, amount => $normalize_number->($amount));
	} @$l ];
    };
    @{$self->{statements}};
}

package Finance::Bank::LaPoste::Statement;

=pod

=head1 Statement methods

=head2 date()

Returns the date when the statement occured, in DD/MM/YY format.

=head2 description()

Returns a brief description of the statement.

=head2 amount()

Returns the amount of the statement (expressed in Euros or the account's
currency). Although the Crédit Mutuel website displays number in continental
format (i.e. with a coma as decimal separator), amount() returns a real
number.

=head2 as_string($separator)

Returns a tab-delimited representation of the statement. By default, it uses
a tabulation to separate the fields, but the user can provide its own
separator.

=cut

sub new {
    my ($class, %statement) = @_;
    bless \%statement, $class;
}

sub description { $_[0]{description} }
sub amount      { $_[0]{amount} }
sub date        { 
    my ($self) = @_;
    my ($year) = $self->{year} =~ /..(..)/; # only 2 digits for year
    "$year/$self->{month}/$self->{day}"
}

sub as_string { 
    my ($self, $separator) = @_;
    join($separator || "\t", $self->date, $self->{description}, $self->{amount});
}

1;

=pod

=head1 COPYRIGHT

Copyright 2002-2007, Pascal 'Pixel' Rigaux. All Rights Reserved. This module
can be redistributed under the same terms as Perl itself.

=head1 AUTHOR

Thanks to Cédric Bouvier for Finance::Bank::CreditMut
(and also to Simon Cozens and Briac Pilpré for various Finance::Bank::*)

=head1 SEE ALSO

Finance::Bank::BNPParibas, Finance::Bank::CreditMut

=cut
