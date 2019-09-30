#![allow(dead_code)]

use std::collections::HashMap;

/// Save positions of individual clients through the buffer history.
///
/// network_id/
///   buffer_id/
///     chunk.log
struct Cursor {
    network_id: String,
    buffer_id: String,
    client_id: String,

    pos: CursorPosition,
}

struct CursorPosition {
    chunk: u64,
    offset: u64,
}

// client_id, buffer_id, network_id
type CursorKey = (String, String, String);

/// On-disk representation of all cursors for each network.
// TODO: serde
type CursorState = HashMap<CursorKey, CursorPosition>;

/// Conceptual representation of a buffer. Internally can paginate
/// over multiple chunks.
struct Buffer {
    network: String
}

// TODO: actual read/write ops to file
struct BufferChunk {}

/// Keep track of where each client is in each buffer
struct BufferManager {
    state: CursorState,
}
