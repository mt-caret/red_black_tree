an implementation of purely functional red-black trees in OCaml.

todo

- [ ] add the rest of the tests from <https://matt.might.net/articles/quick-quickcheck>
- [ ] "Construct Red-Black Trees"
- [ ] "Efficient Verified Red-Black Trees"
- [ ] "Deletion: The Curse of the Red-Black Tree"
- [ ] benchmark (against Core maps, HAMTs, [purely functional B-trees?](https://news.ycombinator.com/item?id=23002849))
  - [] HAMTs
    - <https://worace.works/2016/05/24/hash-array-mapped-tries/>
    - "Ideal Hash Trees"
- [ ] "A Pedagogically Sound yet Efficient Deletion algorithm for Red-Black Trees: The Parity-Seeking Delete Algorithm"
- [ ] "Functional Algorithms, Verified!"
- [ ] "Experience Report: Type-Driven Development of Certified Tree Algorithms in Coq"
- [ ] "Red-Black Trees with Types"
- [ ] benchmark properly (flambda, proper machine, etc.)

notes

- presumably red-black trees require less rebalancing, which seems to cut
  against in the purely functional setting; can we replicate pathological cases
  which require rebalancing over and over again?  look to "Construct Red-Black
  Trees" to understand issue better

benchmarks

```
Estimated testing time 40s (4 benchmarks x 10s). Change using '-quota'.

  Name                     Time/Run      mWd/Run   mjWd/Run   Prom/Run   Percentage
 ------------------------ ---------- ------------ ---------- ---------- ------------
  Red_black_tree.of_list     2.33ms   1_121.05kw    70.77kw    70.77kw       85.59%
  Core.Set.of_list           2.73ms     941.54kw    60.61kw    60.61kw      100.00%
  Red_black_tree.mem         1.08ms                                          39.55%
  Core.Set.mem               1.35ms                                          49.33%

```
