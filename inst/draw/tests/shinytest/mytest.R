requireNamespace("shinytest", quietly = TRUE)
app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$snapshot()
