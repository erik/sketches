# panamint

Experimental routing engine for OpenStreetMap data with support for custom
profiles.

```
profile "custom" {
    # Penalty for passing through a node
    node-penalty {
        when {
            [barrier=gate] => 10
            else           => 0
        }
    }

    # Penalty for traversing way
    way-penalty {
        when {
            [route=ferry] => 100
            else          => 0
        }
    }

    # The "cost" of traversing edge of the graph, scaled by the length
    # of the edge.
    cost-factor {
        define {
            surface-penalty = when {
                [surface=paved]   => 0
                [surface=unpaved] => 1
                else              => 0.5
            }

            access-penalty = when {
                [access=private|no] => invalid
                [bicycle=dismount]  => 6
                else                => 0
            }
        }

        sum {
            surface-penalty
            access-penalty
        }
    }
}
```

For a full example, see [cxb.mint](./profiles/cxb.mint), which is a port of
the custom BRouter profile used by [cxberlin](https://routing.cxberlin.net/).