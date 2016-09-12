## ------------------------------------------------------------------------
library(mstate)
data(aidssi)
ci <- Cuminc(time=aidssi$time, status=aidssi$status)
ggplotit(ci)
ggplotit(ci, conf.int=TRUE)
ggplotit(ci, conf.int=TRUE, labels=c("event-free","AIDS","SI"))

ci <- survfit(Surv(time=aidssi$time, event=aidssi$status, type="mstate")~1)
ggplotit(ci)

ci <- survfit(Surv(time=aidssi$time, event=aidssi$status, type="mstate")~aidssi$ccr5)
ggplotit(ci)

ggplotit(ci) + theme_bw()

ggplotit(ci) + theme_bw() + facet_wrap(~strata)


