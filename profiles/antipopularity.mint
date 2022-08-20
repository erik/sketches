profile "avoid personal popularity, prefer global" {
  define {
    pop-self-weight = 2
    pop-global-weight = 1.0
  }

  node-penalty {
    0
  }

  way-penalty {
    0
  }

  cost-factor {
    define {
      self-pop-penalty = mul {
        pop-self-weight
        way.popularity-self
      }

      global-pop-weight = mul {
        pop-global-weight;
        way.popularity-self
      }

      pop-penalty = div {
          self-pop-penalty
          sum { global-pop-weight; 0.01 }
        }
    }

    sub {
      pop-penalty
      global-pop-weight
    }
  }
}
