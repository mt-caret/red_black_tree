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
Estimated testing time 1m20s (8 benchmarks x 10s). Change using '-quota'.

  Name                           Time/Run      mWd/Run     mjWd/Run     Prom/Run   Percentage
 ------------------------ ---------------- ------------ ------------ ------------ ------------
  Red_black_tree.of_list   2_417_529.01ns   1_121.05kw   70_774.53w   70_774.53w       67.93%
  Core.Set.of_list         2_935_892.63ns     941.54kw   60_623.57w   60_623.57w       82.49%
  Red_black_tree.length       28_950.83ns                                               0.81%
  Core.Set.length                  2.82ns
  Red_black_tree.mem       1_148_047.15ns                                              32.26%
  Core.Set.mem             1_407_632.37ns                                              39.55%
  Red_black_tree.add       1_961_018.33ns   1_200.00kw      171.53w      171.53w       55.10%
  Core.Set.add             3_559_006.03ns   1_140.00kw      219.81w      219.81w      100.00%

```
