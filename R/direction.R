#' Directions from data cells to headers
#'
#' @description
#'
#' How to use functions with a `direction` parameter.
#'
#' Data cells relate to header cells by their proximity in a given direction.
#' The point of view is always *from* the data cell *to* the header.  For
#' example, the direction `"up"` means "from each data cell go directly up to a
#' cell at the edge of the table, which is the header cell."
#'
#' Scroll down to the "Tables" section for a visual explanation.
#'
#' Legacy directions `"N"`, `"NNW", `"W"`, `"WNW"`, etc. are still supported.
#' Scroll down to the "Legacy directions" section for how they map to the new
#' directions.
#'
#' * `"up"` means from each data cell go directly up to a cell at the edge of
#' the the table, which is the header cell.
#' * `"up-left"` means from each data cell go directly up to a cell at the edge
#' of the the table, then if the cell is blank go left until a cell that has a
#' value, which is the header cell.
#' * `"up-right"` means from each data cell go directly up to a cell at the edge
#' of the the table, then if the cell is blank go right until a cell that has a
#' value, which is the header cell.
#'
#' * `"left"` means from each data cell go directly left to a cell at the edge
#' of the the table, which is the header cell.
#' * `"left-up"` means from each data cell go directly left to a cell at the
#' edge of the the table, then if the cell is blank go up until a cell that has
#' a value, which is the header cell.
#' * `"left-down"` means from each data cell go directly left to a cell at the
#' edge of the the table, then if the cell is blank go down until a cell that
#' has a value, which is the header cell.
#'
#' * `"right"` means from each data cell go directly right to a cell at the edge
#' of the the table, which is the header cell.
#' * `"right-up"` means from each data cell go directly right to a cell at the
#' edge of the the table, then if the cell is blank go up until a cell that has
#' a value, which is the header cell.
#' * `"right-down"` means from each data cell go directly right to a cell at the
#' edge of the the table, then if the cell is blank go down until a cell that
#' has a value, which is the header cell.
#'
#' * `"down"` means from each data cell go directly down to a cell at the edge
#' of the the table, which is the header cell.
#' * `"down-left"` means from each data cell go directly down to a cell at the
#' edge of the the table, then if the cell is blank go left until a cell that
#' has a value, which is the header cell.
#' * `"down-right"` means from each data cell go directly down to a cell at the
#' edge of the the table, then if the cell is blank go right until a cell that
#' has a value, which is the header cell.
#'
#' @section -ish:
#'
#' The difference between `"up"` and `"up-ish"` (and similar pairs of directions)
#' is that `"up"` finds headers directly above the data cell, whereas `"up-ish"`
#' matches the nearest header, whether above-left, above-right or directly above
#' the data cell.  This is useful for matching headers that are not aligned to
#' the edge of the data cells that they refer to.  There can be a tie in the
#' directions `"up-ish"`, `"down-ish"`, `"left-ish"` and `"right-ish"` , causing
#' `NA`s to be returned in the place of header values.  Avoid ties by using
#' [justify()] first to align header cells to the corner of the data cells they
#' describe.
#'
#' * `"up-ish"` means the closest cell at the top edge of the table without
#' crossing a border defined by the `border` parameter.
#' * `"left-ish"` means the closest cell at the left-hand edge of the table
#' without crossing a border defined by the `border` parameter.
#' * `"right-ish"` means the closest cell at the right-hand edge of the table
#' without crossing a border defined by the `border` parameter.
#' * `"down-ish"` means the closest cell at the bottom edge of the table without
#' crossing a border defined by the `border` parameter.
#'
#' @section Tables:
#'
#' ```
#' +----------------+-------------+-------------+
#' |                | up-left     | up-left     |
#' +                +-------------+-------------+
#' |                | up   | up   | up   | up   |
#' +----------------+------+------+------+------+
#' | left-up | left | data | data | data | data |
#' +         +------+------+------+------+------+
#' |         | left | data | data | data | data |
#' +---------+------+------+------+------+------+
#' | left-up | left | data | data | data | data |
#' +         +------+------+------+------+------+
#' |         | left | data | data | data | data |
#' +---------+------+------+------+------+------+
#' ```
#'
#' ```
#' +-------------+-------------+------------------+
#' | up-right    | up-right    |                  |
#' +-------------+-------------+                  +
#' | up   | up   | up   | up   |                  |
#' +------+------+------+------+------------------+
#' | data | data | data | data | right | right-up |
#' +------+------+------+------+-------+          +
#' | data | data | data | data | right |          |
#' +------+------+------+------+-------+----------+
#' | data | data | data | data | right | right-up |
#' +------+------+------+------+-------+          +
#' | data | data | data | data | right |          |
#' +------+------+------+------+-------+----------+
#' ```
#'
#' ```
#' +-----------+------+------+------+------+------+
#' |           | left | data | data | data | data |
#' +           +------+------+------+------+------+
#' | left-down | left | data | data | data | data |
#' +-----------+------+------+------+------+------+
#' |           | left | data | data | data | data |
#' +           +------+------+------+------+------+
#' | left-down | left | data | data | data | data |
#' +-----------+------+------+------+------+------+
#' |                  | down | down | down | down |
#' +                  +------+------+------+------+
#' |                  | down-left   | down-left   |
#' +-----------+------+-------------+-------------+
#' ```
#'
#' ```
#' '+------+------+------+------+-------+------------+
#' | data | data | data | data | right |            |
#' +------+------+------+------+-------+            +
#' | data | data | data | data | right | right-down |
#' +------+------+------+------+-------+------------+
#' | data | data | data | data | right |            |
#' +------+------+------+------+-------+            +
#' | data | data | data | data | right | right-down |
#' +------+------+------+------+-------+------------+
#' | down | down | down | down |                    |
#' +------+------+------+------+                    +
#' |  down-right |  down-right |                    |
#' +-------------+-------------+--------------------+
#' ```
#'
#' ```
#' +-----------------+----------------------+-----------------------------+
#' |                 |        up-ish        |               up-ish        |
#' +                 +----------------------+-----------------------------+
#' |                 | up   | up     | up   | up   | up   | up     | up   |
#' +-----------------+------+--------+------+------+------+--------+------+
#' |          | left | data | data   | data | data | data | data   | data |
#' +          +------+------+--------+------+------+------+--------+------+
#' | left-ish | left | data | data   | data | data | data | data   | data |
#' +          +------+------+--------+------+------+------+--------+------+
#' |          | left | data | data   | data | data | data | data   | data |
#' +----------+------+------+--------+------+------+------+--------+------+
#' |          | left | data | data   | data | data | data | data   | data |
#' +          +------+------+--------+------+------+------+--------+------+
#' |          | left | data | data   | data | data | data | data   | data |
#' +          +------+------+--------+------+------+------+--------+------+
#' | left-ish | left | data | data   | data | data | data | data   | data |
#' +          +------+------+--------+------+------+------+--------+------+
#' |          | left | data | data   | data | data | data | data   | data |
#' +----------+------+------+--------+------+------+------+-----  -+------+
#' ```
#'
#' ```
#' +------+----------+------+------+------+----------+------+-------+-----------+
#' | data | data     | data | data | data | data     | data | right |           |
#' +------+----------+------+------+------+----------+------+-------+           +
#' | data | data     | data | data | data | data     | data | right | right-ish |
#' +------+----------+------+------+------+----------+------+-------+           +
#' | data | data     | data | data | data | data     | data | right |           |
#' +------+----------+------+------+------+----------+------+-------+-----------+
#' | data | data     | data | data | data | data     | data | right |           |
#' +------+----------+------+------+------+----------+------+-------+           +
#' | data | data     | data | data | data | data     | data | right |           |
#' +------+----------+------+------+------+----------+------+-------+           +
#' | data | data     | data | data | data | data     | data | right | right-ish |
#' +------+----------+------+------+------+----------+------+-------+           +
#' | data | data     | data | data | data | data     | data | right |           |
#' +------+----------+------+------+------+----------+------+-------+-----------+
#' | down | down     | down | down | down | down     | down |                   |
#' +------+----------+------+------+------+----------+------+                   +
#' |        down-ish        |               down-ish        |                   |
#' +------------------------+-------------------------------+-------------------+
#' ```
#'
#' @section Legacy directions:
#'
#' Older versions of unpivotr used different names for the directions, based on
#' the points of the compass.  These are still supported but are discouraged.
#'
#' ```
#' | old direction | new direction |
#' |---------------|---------------|
#' | N             | up            |
#' | NNW           | up-left       |
#' | NNE           | up-right      |
#' | W             | left          |
#' | WNW           | left-up       |
#' | WSW           | left-down     |
#' | E             | right         |
#' | ENE           | right-up      |
#' | ESE           | right-down    |
#' | S             | down          |
#' | SSW           | down-left     |
#' | SSE           | down-right    |
#' ```
#'
#' @name direction
NULL
