profile "cxb - gravel optimized" {
  define {
    avoid-highways? = true
    avoid-path?     = true

    left-turn-cost = 150
  }

  way-penalty {
    define {
        cycleway-on-right? = when {
            [reversedirection=yes] => [cycleway:left=track|lane|shared_lane]
            else                   => [cycleway:right=track|lane|shared_lane]
        }

        cycleway? = any? {
            [cycleway; cycleway!=no]
            cycleway-on-right?
        }

        cycleroute? = any? {
            [route_bicycle_icn=yes]
            [route_bicycle_ncn=yes]
            [route_bicycle_rcn=yes]
            [route_bicycle_lcn=yes]
        }

        paved?       = [surface=paved|asphalt]
        unpaved?     = [surface=unpaved|gravel|dirt|earth|ground|sand]
        fine-gravel? = [surface=fine_gravel|cobblestone|compacted|paving_stones]
        concrete?    = [surface=concrete]

        default-access? = any? {
            [!access; motorroad!=yes]
            [access!=private|no]
        }

        bike-access? = any? {
            [bicycle!=private|no|dismount]
            [vehicle!=private|no]
            cycleroute?
            default-access?
        }

        foot-access? = any? {
            bike-access?
            [bicycle=dismount]
            [foot!=private|no]
            default-access?
        }

        access-penalty = when {
            bike-access? => 0
            foot-access? => 6
            else         => invalid
        }

        estimated-surface-penalty = when {
            // Very likely to be asphalt
            [highway=primary|primary_link|secondary|secondary_link|tertiary|tertiary_link] => 1.5
            // Likely to be asphalt
            [highway=road|residential|unclassified]  => 1.0
            // Something paved, might be good?
            [highway=living_street|service|cycleway] => 1.0
            [smoothness=intermediate|good|excellent] => 0
            [tracktype=grade1]                       => 0.9
            [tracktype=grade2]                       => 0
            cycleroute?                              => 3.5
            else                                     => 0
        }

        // TODO: cycleway:surface
        surface-penalty = when {
            [surface=fine_gravel|compacted] => 0
            [surface=pebblestone|unpaved]   => 0.1
            [surface=concrete|paving_stones|wood|metal|ground|grass|dirt|earth|mud|clay|gravel|sand]
                                            => 0.5
            [surface=grass_paver]           => 0.3
            [surface=asphalt]               => 1.5
            [concrete=plates|lanes]         => 1.5

            [surface=sett] => when {
                [smoothness=good|excellent] => 0.1
                else                        => 0.3
            }
            [surface=paved] => when {
                [smoothness=good|excellent] => 0.3
                else                        => 0.4
            }
            [surface=cobblestone] => when {
                [smoothness=good|excellent] => 1
                else                        => 2
            }

            // Exists, but no idea. Likely unpaved.
            [surface]                       => 0.3

            // Have to guess
            else                            =>  estimated-surface-penalty
        }

        track-type-penalty = when {
            [tracktype=grade1] => 0.2
            [tracktype=grade2] => 0
            [tracktype=grade3] => 0.1
            [tracktype=grade4] => 1
            [tracktype=grade5] => 2
            else               => 0
        }

        // TODO: Traffic penalty, max speed penalty, oneway penalty
        smoothness-penalty = when {
            [smoothness=excellent]    => 0.9
            [smoothness=good]         => 0.8
            [smoothness=intermediate] => 0
            [smoothness=bad]          => 1
            [smoothness=very_bad]     => 2
            [smoothness=horrible]     => 4.5
            else                      => 0
        }

        use-sidepath-penalty = when {
            [bicycle=use_sidepath] => when {
                avoid-path? => 0
                else => 0.5
            }
            else => 0
        }

        no-cycleroute-penalty = when {
            cycleroute? => 0
            else        => 0.05
        }

        designation-penalty = when {
            [bicyle=designated] => 0
            [foot=designated]   => 1
            else                => 0.1
        }

        segregated-penalty = when {
            [segregated=no]     => 0.5
            else                => 0
        }

        highway-penalty = when {
            [route=ferry]                            => invalid
            [highway=proposed|abandoned|motorway|motorway_link]
                                                     => invalid
            [highway=trunk|trunk_link]               => 30
            [highway=primary|primary_link]           => 2.6
            [highway=secondary|secondary_link]       => 2.5
            [highway=tertiary|tertiary_link]         => 2.4
            [highway=unclassified]                   => 1.5
            [highway=pedestrian]                     => 8
            [highway=steps]                          => 100
            [highway=bridleway]                      => 31
            [highway=cycleway; foot=yes]             => 2
            [highway=cycleway]                       => 1.5
            [highway=living_street]                  => 1.5
            [highway=residential; bicycle_road=yes]  => 1.1
            [highway=residential]                    => 1.5
            [highway=service]                        => when {
                any? { [service] [service=alley] } => 1.1
                else                               => 11
            }
            [highway=track|road]                     => 1
            [highway=path]                           => when { avoid-path? => 2; else => 1 }
            [highway=footway; bicycle=yes ]          => 1.5
            [highway=footway]                        => 4.7
            else                                     => 19.9
        }
    }

    sum {
        highway-penalty
        surface-penalty
        track-type-penalty
        smoothness-penalty
        use-sidepath-penalty
        no-cycleroute-penalty
        segregated-penalty
        designation-penalty
    }

  }
}
