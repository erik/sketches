#[derive(Clone, Debug)]
struct Positioned {
    x: u32,
}

impl Positioned {
    fn dist_to(&self, other: &Positioned) -> u32 {
        if self.x > other.x {
            self.x - other.x
        } else {
            other.x - self.x
        }
    }
}

// Implemented using a Vantage Point tree
#[derive(Debug)]
struct LocationIndex {
    nodes: Vec<Positioned>,
}

const MAX_POINTS_PER_LEAF_NODE: usize = 2;

type PositionWithDist = (u32, Positioned);

impl LocationIndex {
    fn build(points: Vec<Positioned>) -> LocationIndex {
        assert!(points.len() > 0, "empty data");

        let initial_pivot = &points[0].clone();

        let mut points_with_dist: Vec<_> = points
            .into_iter()
            .map(|pt| (pt.dist_to(initial_pivot), pt))
            .collect();

        Self::build_inner(&mut points_with_dist[..]);

        LocationIndex {
            nodes: points_with_dist.into_iter().map(|(_, p)| p).collect(),
        }
    }

    fn build_inner(data: &mut [PositionWithDist]) {
        if data.len() <= MAX_POINTS_PER_LEAF_NODE {
            return;
        }

        let pivot_idx = data.len() / 2;

        // `select_nth_unstable_by` partially sorts the slice, so that
        // `pivot_idx` is left in the correct sorted position.
        //
        // The rest of the data is partitioned into buckets of "less
        // than pivot" and "greater than pivot"
        let (before, (_, pivot), after) =
            data.select_nth_unstable_by(pivot_idx, |(a, _), (b, _)| a.cmp(b));

        Self::build_inner(before);

        for (dist, point) in &mut after[..] {
            *dist = point.dist_to(pivot);
        }

        Self::build_inner(after);
    }

    fn find_nearest(&self, point: &Positioned) -> Option<&Positioned> {
        let mut nearest = None;
        self.find_nearest_inner(point, 0, self.nodes.len(), &mut nearest);

        // TODO: Check dist within bounds
        if let Some((_dist, index)) = nearest {
            return Some(&self.nodes[index]);
        }

        None
    }

    fn find_nearest_inner(
        &self,
        query_point: &Positioned,
        start: usize,
        end: usize,
        nearest: &mut Option<(u32, usize)>,
    ) {
        if end - start <= MAX_POINTS_PER_LEAF_NODE {
            for i in start..end {
                println!("examining: {:?}", self.nodes[i]);
                let dist = query_point.dist_to(&self.nodes[i]);

                match nearest {
                    None => *nearest = Some((dist, i)),
                    Some((cur_dist, _)) if dist < *cur_dist => *nearest = Some((dist, i)),
                    _ => {}
                }
            }
        } else {
            let pivot = &self.nodes[start];
            let mid_point = start + (end - start) / 2;

            let dist_to_query = pivot.dist_to(query_point);
            let dist_to_boundary = pivot.dist_to(&self.nodes[mid_point]);

            if dist_to_query >= dist_to_boundary {
                self.find_nearest_inner(query_point, mid_point, end, nearest);

                let cur_best = nearest.map(|(d, _)| d).unwrap_or(u32::MAX);
                if dist_to_query - dist_to_boundary < cur_best {
                    self.find_nearest_inner(query_point, start, mid_point, nearest);
                }
            }

            if dist_to_query < dist_to_boundary {
                self.find_nearest_inner(query_point, start, mid_point, nearest);

                let cur_best = nearest.map(|(d, _)| d).unwrap_or(u32::MAX);
                if dist_to_boundary - dist_to_query < cur_best {
                    self.find_nearest_inner(query_point, mid_point, end, nearest);
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn construct_tree() {
        let input = vec![1u32, 5, 2, 3, 9, 100, 2, 50]
            .into_iter()
            .map(|x| Positioned { x })
            .collect::<Vec<_>>();

        let index = LocationIndex::build(input);

        println!("index = {:?}", index);

        let neighbor = index.find_nearest(&Positioned { x: 76 });
        assert_eq!(neighbor.unwrap().x, 100);

        let neighbor = index.find_nearest(&Positioned { x: 6 });
        assert_eq!(neighbor.unwrap().x, 5);
    }
}
