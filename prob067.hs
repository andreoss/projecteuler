use v5;
use Modern::Perl;
use List::Util 'reduce';
use Graph;
# https://projecteuler.net/project/resources/p067_triangle.txt

my $graph = Graph::Undirected->new;

my @tri = map { [ grep /\d+/, split /\s+/, $_ ] }
  ( grep { $_ !~ /^\s*$/ } split /\n+/, $triangle );

# Psuedo TOP
$graph->add_weighted_edge( node_name( 0, $_ ), "TOP", 0 )
  for ( 0 .. $#{ $tri[0] } );

# Psuedo BOTTOM
$graph->add_weighted_edge( node_name( $#tri, $_ ),
    "BOTTOM", 100 - $tri[$#tri][$_] )
  for ( 0 .. $#{ $tri[$#tri] } );

sub node_name {
    my ( $i, $k ) = @_;
    return "$tri[$i][$k] " . chr( $i + 65 ) . chr( 65 + $k );
}
for my $i ( 0 .. $#tri ) {
    for my $k ( 0 .. $#{ $tri[$i] } ) {
        $graph->add_weighted_edge(
            node_name( $i,     $k ),
            node_name( $i - 1, $k ),
            100 - $tri[ $i - 1 ][$k]
        ) if $tri[ $i - 1 ][$k] && $i > 0;
        $graph->add_weighted_edge(
            node_name( $i,     $k ),
            node_name( $i - 1, $k - 1 ),
            100 - $tri[ $i - 1 ][ $k - 1 ]
        ) if $tri[ $i - 1 ][ $k - 1 ] && $k > 0 && $i > 0;
    }
}

if ( my @sptg = $graph->SP_Dijkstra( "TOP", "BOTTOM" ) ) {
    say reduce { $a + $b } ( grep /\d/, split /[^\d]+/, "@sptg" );
}



