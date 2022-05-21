
struct CsrNode {
    // Implicit in location: id: u32,
    edge_offset: u32,
    packed_tags: CsrNodeTags,
}

struct CsrEdge {
    // Implicit in construction: from_node_id: u32
    to_node_id: u32,
    packed_tags: CsrEdgeTags,
}

enum EncodedHighway {
    Residential,
    Service,
    Track,
    Footway,
    Path,
    Tertiary,
    Crossing,
    Secondary,
    Primary,
    TurningCircle,
    LivingStreet,
    TrafficSignals,
    Trunk,
    Cycleway,
    Steps,
    Stop,
    Motorway,
    MotorwayLink,
    GiveWay,
    Pedestrian,
    TrunkLink,
    PrimaryLink,
    SecondaryLink,
    Construction,
    TertiaryLink,
    MotorwayJunction,
    TurningLoop,
    Proposed,
    Bridleway,
    Road,
    MiniRoundabout,
    Trailhead,
}

enum EncodedSurface {
    Asphalt,
    Unpaved,
    Paved,
    Ground,
    Concrete,
    PavingStones,
    Gravel,
    Dirt,
    Grass,
    Compacted,
    Sand,
    Sett,
    FineGravel,
    Cobblestone,
    Wood,
    Earth,
    ConcretePlates,
    Pebblestone,
    Metal,
    GrassPaver,
    Clay,
    Tartan,
    ArtificialTurf,
    Mud,
    ConcreteLanes,
    UnhewnCobblestone,
    Rock,
}

struct CsrNodeTags {
    highway: EncodedHighway
}

// Compressed Sparse Row
struct GraphStorage {
    nodes: Vec<CsrNode>,
    edges: Vec<CsrEdge>,
}

fn main() {
    println!("Hello, world!");
}
