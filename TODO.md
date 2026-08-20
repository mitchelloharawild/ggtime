* Add personal travel geom_time_line() transition with hourly step counts around the world.

## coord_calendar - defaults
- The suite was red before I started (6 failures). panes shipped defaulting to mixtime::months(1L), which contradicts both the plan and the NEWS.md entry already written for it ("It defaults to NULL"), and that default tripped its own check_pane_granule() validation whenever cols was finer than a month. I set it to NULL as documented, which fixed all six. If the months default was deliberate, the validation needs to distinguish a user-set panes from the default.
- One old test (a block rule replaces the row rule…) relied on a block rule appearing without setting blocks; it now passes blocks = mixtime::months(1L) explicitly.