an implementation of purely functional red-black trees in OCaml.

todo

- [ ] add the rest of the tests from <https://matt.might.net/articles/quick-quickcheck>
- [x] "Constructing Red-Black Trees"
- [ ] "Efficient Verified Red-Black Trees"
- [ ] "Deletion: The Curse of the Red-Black Tree"
- [ ] benchmark (against Core maps, HAMTs, [purely functional B-trees?](https://news.ycombinator.com/item?id=23002849))
  - [ ] HAMTs
    - <https://worace.works/2016/05/24/hash-array-mapped-tries/>
    - "Ideal Hash Trees"
- [ ] "A Pedagogically Sound yet Efficient Deletion algorithm for Red-Black Trees: The Parity-Seeking Delete Algorithm"
- [ ] "Functional Algorithms, Verified!"
- [ ] "Experience Report: Type-Driven Development of Certified Tree Algorithms in Coq"
- [ ] "Red-Black Trees with Types"
- [ ] benchmark properly (flambda, proper machine, etc.)
- [ ] [denotational homomorphic testing](https://www.tweag.io/blog/2021-10-13-homomorphic-testing/) (lol)
- [ ] randomize the order of insertion into the tree in benchmarks?
- [ ] <https://hypirion.com/musings/understanding-persistent-vector-pt-1>

benchmarks

```
Estimated testing time 1m40s (10 benchmarks x 10s). Change using '-quota'.

  Name                                                    Time/Run      mWd/Run     mjWd/Run     Prom/Run   Percentage
 ------------------------------------------------- ---------------- ------------ ------------ ------------ ------------
  Red_black_tree.of_list                            2_398_297.12ns   1_121.05kw   70_782.62w   70_782.62w       74.03%
  Core.Set.of_list                                  2_646_818.01ns     941.54kw   60_624.97w   60_624.97w       81.70%
  Red_black_tree.of_increasing_iterator_unchecked     117_336.59ns      50.00kw    5_123.07w    5_123.07w        3.62%
  Set.of_increasing_iterator_unchecked                 92_543.91ns      43.62kw    3_806.15w    3_806.15w        2.86%
  Red_black_tree.length                                18_904.72ns                                               0.58%
  Core.Set.length                                           2.21ns
  Red_black_tree.mem                                1_103_910.96ns                                              34.08%
  Core.Set.mem                                      1_295_415.03ns                                              39.99%
  Red_black_tree.add                                1_927_900.32ns   1_200.00kw      171.76w      171.76w       59.51%
  Core.Set.add                                      3_239_567.65ns   1_140.00kw      219.92w      219.92w      100.00%

```
