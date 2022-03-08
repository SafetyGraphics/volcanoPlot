
# makes dplyr functions not have conflict
select <- dplyr::select; rename <- dplyr::rename; mutate <- dplyr::mutate; summarize <- dplyr::summarize; 
arrange <- dplyr::arrange; slice <- dplyr::slice; filter <- dplyr::filter; recode<-dplyr::recode; first<-dplyr::first
style <- plotly::style


reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv,
            log_breaks(base = base),
            domain = c(1e-100, Inf))
}
