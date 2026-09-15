# DataClinicalHarmonization

# dsClinicalDataHarmonization -- DataSHIELD method registration
#
# Disclosure control convention used throughout this package:
#   - Functions that CREATE OR MODIFY a data object are TRUE assign methods:
#     they return the transformed data frame directly and MUST be called via
#     datashield.assign.expr(), never datashield.aggregate(). This is what
#     both (a) persists the object correctly in the DataSHIELD session, so
#     it's visible afterwards to any other DataSHIELD function including
#     dsBaseClient's own (ds.summary, ds.dim, etc.), and (b) guarantees
#     disclosure safety, since Opal/DSLite NEVER transmits an assign call's
#     return value to the client -- this is an infrastructure-level
#     guarantee, independent of what the function itself returns. Any small
#     summary about what an assign function did (e.g. which columns were
#     converted, how many rows were dropped) is attached as an ATTRIBUTE on
#     the returned data frame (invisible to normal use -- is.data.frame(),
#     nrow(), ds.summary(), etc. are unaffected) and retrieved afterwards by
#     a small companion "get_..." AGGREGATE function that reads the
#     attribute back off the now-properly-persisted object.
#   - Every other function (checks, statistics, the "get_..." companions)
#     never creates or returns row-level data and is a plain aggregate
#     method.

