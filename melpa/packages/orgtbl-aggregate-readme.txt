A new org-mode table is automatically updated,
based on another table acting as a data source
and user-given specifications for how to perform aggregation.

Example:
Starting from a source table of activities and quantities
(whatever they are) over several days,

#+TBLNAME: original
| Day       | Color | Level | Quantity |
|-----------+-------+-------+----------|
| Monday    | Red   |    30 |       11 |
| Monday    | Blue  |    25 |        3 |
| Thuesday  | Red   |    51 |       12 |
| Thuesday  | Red   |    45 |       15 |
| Thuesday  | Blue  |    33 |       18 |
| Wednesday | Red   |    27 |       23 |
| Wednesday | Blue  |    12 |       16 |
| Wednesday | Blue  |    15 |       15 |
| Turdsday  | Red   |    39 |       24 |
| Turdsday  | Red   |    41 |       29 |
| Turdsday  | Red   |    49 |       30 |
| Friday    | Blue  |     7 |        5 |
| Friday    | Blue  |     6 |        8 |
| Friday    | Blue  |    11 |        9 |

an aggregation is built for each day (because several rows
exist for each day), typing C-c C-c

#+BEGIN: aggregate :table original :cols "Day mean(Level) sum(Quantity)"
| Day       | mean(Level) | sum(Quantity) |
|-----------+-------------+---------------|
| Monday    |        27.5 |            14 |
| Thuesday  |          43 |            45 |
| Wednesday |          18 |            54 |
| Turdsday  |          43 |            83 |
| Friday    |           8 |            22 |
#+END

A wizard can be used:
M-x org-insert-dblock:aggregate

Full documentation here:
  https://github.com/tbanel/orgaggregate/blob/master/README.org
