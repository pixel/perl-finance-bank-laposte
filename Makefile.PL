use ExtUtils::MakeMaker;
WriteMakefile(
	NAME          => 'Finance::Bank::LaPoste',
	VERSION_FROM  => 'lib/Finance/Bank/LaPoste.pm',
	ABSTRACT_FROM => 'lib/Finance/Bank/LaPoste.pm',
	AUTHOR        => 'Pascal Pixel Rigaux <pixel@rigaux.org>',
	PREREQ_PM     => { 'HTTP::Cookies' => 0, 'HTML::Parser' => 0 },
);
