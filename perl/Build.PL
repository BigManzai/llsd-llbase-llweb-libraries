use strict;
use warnings;
use Module::Build;
use Module::Build::Compat;

my $builder = Module::Build->new(
    module_name         => 'LLSD',
    license             => 'mit',
    dist_author         => 'Joshua Bell <josh@lindenlab.com>',
    dist_version        => '2.0',
    requires => {
        'Date::Parse' => '2.27',
        'Exporter' => '5.58',
        'MIME::Base64' => '3.07',
        'Scalar::Util' => '1.18',
        'XML::DOM' => '1.43',
        'overload' => '1.04'
    },
    build_requires => {
        'Test::More' => '0.10'
    },
    add_to_cleanup      => [ 'Indra-*' ],
    create_makefile_pl => 'traditional'
);

$builder->create_build_script();
Module::Build::Compat->create_makefile_pl('small',$builder);
