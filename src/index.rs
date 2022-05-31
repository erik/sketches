pub trait Coordinate {
    fn dist_to(&self, other: &Self) -> f32;
}

// Implemented using a Vantage Point tree
#[derive(Debug)]
pub struct SpatialIndex<Coord, Ix> {
    tree: Vec<(Coord, Ix)>,
}

const MAX_POINTS_PER_LEAF_NODE: usize = 8;

impl<Coord, Ix> SpatialIndex<Coord, Ix>
where
    Ix: Copy,
    Coord: Coordinate + Copy,
{
    pub fn build(points: Vec<(Coord, Ix)>) -> SpatialIndex<Coord, Ix> {
        assert!(points.len() > 0, "empty data");

        let initial_vp = &points[0].0.clone();

        let mut points_with_dist: Vec<_> = points
            .into_iter()
            .map(|(pt, ix)| (pt.dist_to(initial_vp), (pt, ix)))
            .collect();

        Self::build_inner(&mut points_with_dist[..]);

        let tree = points_with_dist.into_iter().map(|(_, p)| p).collect();

        SpatialIndex { tree }
    }

    fn build_inner(data: &mut [(f32, (Coord, Ix))]) {
        if data.len() <= MAX_POINTS_PER_LEAF_NODE {
            return;
        }

        let pivot_idx = data.len() / 2;

        // `select_nth_unstable_by` partially sorts the slice, so that
        // `data[pivot_idx]` is left in the correct sorted position.
        //
        // The rest of the data is partitioned into buckets of "less
        // than pivot" and "greater than pivot"
        //
        // TODO: Why pivot_idx - 1?
        let (within_vp, (vp_dist, new_vp), outside_vp) =
            data.select_nth_unstable_by(pivot_idx - 1, |(a, _), (b, _)| a.partial_cmp(b).unwrap());

        Self::build_inner(within_vp);

        *vp_dist = 0.0;
        for (dist, (point, _ix)) in &mut outside_vp[..] {
            *dist = point.dist_to(&new_vp.0);
        }

        Self::build_inner(outside_vp);
    }

    pub fn find_nearest_within(&self, point: &Coord, radius: f32) -> Option<Ix> {
        let mut nearest_so_far = (radius, None);
        Self::find_nearest_inner(&self.tree, point, &mut nearest_so_far);
        return nearest_so_far.1.map(|index| index);
    }

    fn find_nearest_inner(
        tree: &[(Coord, Ix)],
        query_point: &Coord,
        nearest_so_far: &mut (f32, Option<Ix>),
    ) {
        if tree.len() <= MAX_POINTS_PER_LEAF_NODE {
            for &(point, index) in &tree[..] {
                let dist = query_point.dist_to(&point);

                if dist < nearest_so_far.0 {
                    *nearest_so_far = (dist, Some(index));
                }
            }
        } else {
            let (vantage_point, _ix) = &tree[0];

            let pivot_idx = tree.len() / 2;

            let dist_to_query = vantage_point.dist_to(query_point);
            let dist_to_boundary = vantage_point.dist_to(&tree[pivot_idx].0);

            if dist_to_query >= dist_to_boundary {
                Self::find_nearest_inner(&tree[pivot_idx..], query_point, nearest_so_far);

                if dist_to_query - dist_to_boundary < nearest_so_far.0 {
                    Self::find_nearest_inner(&tree[..pivot_idx], query_point, nearest_so_far);
                }
            } else {
                Self::find_nearest_inner(&tree[..pivot_idx], query_point, nearest_so_far);

                if dist_to_boundary - dist_to_query < nearest_so_far.0 {
                    Self::find_nearest_inner(&tree[pivot_idx..], query_point, nearest_so_far);
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    impl Coordinate for f32 {
        fn dist_to(&self, other: &Self) -> u32 {
            (self - other).abs() as u32
        }
    }

    impl Coordinate for (f32, f32) {
        fn dist_to(&self, other: &Self) -> u32 {
            let a = self.0 - other.0;
            let b = self.1 - other.1;

            (a * a + b * b).sqrt() as u32
        }
    }

    #[test]
    fn construct_tree() {
        let input = vec![0.0, 1.0, 3.0, 5.0, 8.0, 13.0, 21.0, 35.0, 56.0, 91.0, 147.0]
            .iter()
            .enumerate()
            .map(|(ix, x)| (*x, ix))
            .collect();
        let index = SpatialIndex::build(input);

        println!("index = {:?}", index);

        let neighbor = index.find_nearest_within(&0.0, u32::MAX);
        assert_eq!(neighbor.unwrap(), 0);
    }

    #[test]
    fn construct_2d_index() {
        let input = vec![
            (0.0, 100.0),
            (0.0, 0.0),
            (1000.0, 1000.0),
            (1000.0, 200.0),
            (1000.0, 500.0),
            (50.0, 50.0),
            (1000.0, 900.0),
            (1000.0, 700.0),
            (1000.0, 800.0),
            (80.0, 80.0),
            (1000.0, 600.0),
            (1000.0, 100.0),
            (1000.0, 300.0),
            (100.0, 100.0),
        ];

        let index = SpatialIndex::build(input.iter().enumerate().map(|(ix, x)| (*x, ix)).collect());

        for x in &input[..] {
            println!("find nearest to {:?}", x);
            let nearest_ix = index.find_nearest_within(x, 10);

            assert_eq!(nearest_ix.map(|ix| input[ix]).unwrap(), *x);
        }
        for x in &input[..] {
            let nearest_ix = index.find_nearest_within(&(x.0 - 1.0, x.1 + 1.0), 10);

            assert_eq!(nearest_ix.map(|ix| input[ix]).unwrap(), *x);
            println!("\n\n\n\n\n");
        }
    }
}
