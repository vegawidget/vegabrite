N = 'nominal';
O = 'ordinal';
Q = 'quantitative';
T = 'temporal';

extLogic = list(
  'equals' = list('arg' = c('equal')),
  'gte' =    list('arg' = c('gte')),
  'gt' =     list('arg' = c('gt')),
  'lte' =    list('arg' = c('lte')),
  'lt' =     list('arg' = c('lt')),
  'oneOf' =  list('arg' = c('...')),
  'inRange' = list('arg' = c('...')),
  'valid' = list('arg' = c('valid'))
)

# -- Transforms --

desc = list(
    "aggregate" ='Group and summarize data as counts, sums, averages, etc.',
    "bin" = 'Discretize numeric values into uniform bins.',
    "calculate" = 'Calculate a new data field value.',
    "filter" = 'Remove data that does not match provided conditions.',
    "flatten" = 'Map array fields to new records, one per array entry.',
    "fold" = 'Collapse one or more data fields into two key, value fields.',
    "impute" = 'Fill in missing values with imputed values.',
    "joinaggregate" = 'Extend input data with aggregate values as new fields.',
    "join" = 'A convenient shorthand for joinaggregate.',
    "lookup" = 'Extend input data with values from another data source.',
    "sample" = 'Filter random records from the data limit its size.',
    "stack" = 'Compute running sums to stack groups of values.',
    "timeUnit" = 'Discretize date/time values into meaningful intervals.',
    "window" = 'Perform running calculations over sorted groups.',
    "groupby" = 'Group by fields for aggregate or window transforms.'
  )

transform <- function(name, def, ...) {
    return(list(
      "desc" = desc[[name]],
      "doc" =  'Data Transformations',
      "def" =  def,
      "arg" =  list(...)
    ))
}


