#
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#' @include generics.R column.R
NULL

#' lit
#'
#' A new \linkS4class{Column} is created to represent the literal value.
#' If the parameter is a \linkS4class{Column}, it is returned unchanged.
#'
#' @family normal_funcs
#' @rdname lit
#' @name lit
#' @export
#' @examples
#' \dontrun{
#' lit(df$name)
#' select(df, lit("x"))
#' select(df, lit("2015-01-01"))
#'}
setMethod("lit", signature("ANY"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "lit",
                              if (class(x) == "Column") { x@jc } else { x })
            column(jc)
          })

#' abs
#'
#' Computes the absolute value.
#'
#' @rdname abs
#' @name abs
#' @family normal_funcs
#' @export
#' @examples \dontrun{abs(df$c)}
setMethod("abs",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "abs", x@jc)
            column(jc)
          })

#' acos
#'
#' Computes the cosine inverse of the given value; the returned angle is in the range
#' 0.0 through pi.
#'
#' @rdname acos
#' @name acos
#' @family math_funcs
#' @export
#' @examples \dontrun{acos(df$c)}
setMethod("acos",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "acos", x@jc)
            column(jc)
          })

#' approxCountDistinct
#'
#' Aggregate function: returns the approximate number of distinct items in a group.
#'
#' @rdname approxCountDistinct
#' @name approxCountDistinct
#' @family agg_funcs
#' @export
#' @examples \dontrun{approxCountDistinct(df$c)}
setMethod("approxCountDistinct",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "approxCountDistinct", x@jc)
            column(jc)
          })

#' ascii
#'
#' Computes the numeric value of the first character of the string column, and returns the
#' result as a int column.
#'
#' @rdname ascii
#' @name ascii
#' @family string_funcs
#' @export
#' @examples \dontrun{\dontrun{ascii(df$c)}}
setMethod("ascii",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "ascii", x@jc)
            column(jc)
          })

#' asin
#'
#' Computes the sine inverse of the given value; the returned angle is in the range
#' -pi/2 through pi/2.
#'
#' @rdname asin
#' @name asin
#' @family math_funcs
#' @export
#' @examples \dontrun{asin(df$c)}
setMethod("asin",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "asin", x@jc)
            column(jc)
          })

#' atan
#'
#' Computes the tangent inverse of the given value.
#'
#' @rdname atan
#' @name atan
#' @family math_funcs
#' @export
#' @examples \dontrun{atan(df$c)}
setMethod("atan",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "atan", x@jc)
            column(jc)
          })

#' avg
#'
#' Aggregate function: returns the average of the values in a group.
#'
#' @rdname avg
#' @name avg
#' @family agg_funcs
#' @export
#' @examples \dontrun{avg(df$c)}
setMethod("avg",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "avg", x@jc)
            column(jc)
          })

#' base64
#'
#' Computes the BASE64 encoding of a binary column and returns it as a string column.
#' This is the reverse of unbase64.
#'
#' @rdname base64
#' @name base64
#' @family string_funcs
#' @export
#' @examples \dontrun{base64(df$c)}
setMethod("base64",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "base64", x@jc)
            column(jc)
          })

#' bin
#'
#' An expression that returns the string representation of the binary value of the given long
#' column. For example, bin("12") returns "1100".
#'
#' @rdname bin
#' @name bin
#' @family math_funcs
#' @export
#' @examples \dontrun{bin(df$c)}
setMethod("bin",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "bin", x@jc)
            column(jc)
          })

#' bitwiseNOT
#'
#' Computes bitwise NOT.
#'
#' @rdname bitwiseNOT
#' @name bitwiseNOT
#' @family normal_funcs
#' @export
#' @examples \dontrun{bitwiseNOT(df$c)}
setMethod("bitwiseNOT",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "bitwiseNOT", x@jc)
            column(jc)
          })

#' cbrt
#'
#' Computes the cube-root of the given value.
#'
#' @rdname cbrt
#' @name cbrt
#' @family math_funcs
#' @export
#' @examples \dontrun{cbrt(df$c)}
setMethod("cbrt",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "cbrt", x@jc)
            column(jc)
          })

#' ceil
#'
#' Computes the ceiling of the given value.
#'
#' @rdname ceil
#' @name ceil
#' @family math_funcs
#' @export
#' @examples \dontrun{ceil(df$c)}
setMethod("ceil",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "ceil", x@jc)
            column(jc)
          })

#' Though scala functions has "col" function, we don't expose it in SparkR
#' because we don't want to conflict with the "col" function in the R base
#' package and we also have "column" function exported which is an alias of "col".
col <- function(x) {
  column(callJStatic("org.apache.spark.sql.functions", "col", x))
}

#' column
#'
#' Returns a Column based on the given column name.
#'
#' @rdname col
#' @name column
#' @family normal_funcs
#' @export
#' @examples \dontrun{column(df)}
setMethod("column",
          signature(x = "character"),
          function(x) {
            col(x)
          })
#' corr
#'
#' Computes the Pearson Correlation Coefficient for two Columns.
#'
#' @rdname corr
#' @name corr
#' @family math_funcs
#' @export
#' @examples \dontrun{corr(df$c, df$d)}
setMethod("corr", signature(x = "Column"),
          function(x, col2) {
            stopifnot(class(col2) == "Column")
            jc <- callJStatic("org.apache.spark.sql.functions", "corr", x@jc, col2@jc)
            column(jc)
          })

#' cov
#'
#' Compute the sample covariance between two expressions.
#'
#' @rdname cov
#' @name cov
#' @family math_funcs
#' @export
#' @examples
#' \dontrun{
#' cov(df$c, df$d)
#' cov("c", "d")
#' covar_samp(df$c, df$d)
#' covar_samp("c", "d")
#' }
setMethod("cov", signature(x = "characterOrColumn"),
          function(x, col2) {
            stopifnot(is(class(col2), "characterOrColumn"))
            covar_samp(x, col2)
          })

#' @rdname cov
#' @name covar_samp
setMethod("covar_samp", signature(col1 = "characterOrColumn", col2 = "characterOrColumn"),
          function(col1, col2) {
            stopifnot(class(col1) == class(col2))
            if (class(col1) == "Column") {
              col1 <- col1@jc
              col2 <- col2@jc
            }
            jc <- callJStatic("org.apache.spark.sql.functions", "covar_samp", col1, col2)
            column(jc)
          })

#' covar_pop
#'
#' Compute the population covariance between two expressions.
#'
#' @rdname covar_pop
#' @name covar_pop
#' @family math_funcs
#' @export
#' @examples
#' \dontrun{
#' covar_pop(df$c, df$d)
#' covar_pop("c", "d")
#' }
setMethod("covar_pop", signature(col1 = "characterOrColumn", col2 = "characterOrColumn"),
          function(col1, col2) {
            stopifnot(class(col1) == class(col2))
            if (class(col1) == "Column") {
              col1 <- col1@jc
              col2 <- col2@jc
            }
            jc <- callJStatic("org.apache.spark.sql.functions", "covar_pop", col1, col2)
            column(jc)
          })

#' cos
#'
#' Computes the cosine of the given value.
#'
#' @rdname cos
#' @name cos
#' @family math_funcs
#' @export
#' @examples \dontrun{cos(df$c)}
setMethod("cos",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "cos", x@jc)
            column(jc)
          })

#' cosh
#'
#' Computes the hyperbolic cosine of the given value.
#'
#' @rdname cosh
#' @name cosh
#' @family math_funcs
#' @export
#' @examples \dontrun{cosh(df$c)}
setMethod("cosh",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "cosh", x@jc)
            column(jc)
          })

#' count
#'
#' Aggregate function: returns the number of items in a group.
#'
#' @rdname count
#' @name count
#' @family agg_funcs
#' @export
#' @examples \dontrun{count(df$c)}
setMethod("count",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "count", x@jc)
            column(jc)
          })

#' crc32
#'
#' Calculates the cyclic redundancy check value  (CRC32) of a binary column and
#' returns the value as a bigint.
#'
#' @rdname crc32
#' @name crc32
#' @family misc_funcs
#' @export
#' @examples \dontrun{crc32(df$c)}
setMethod("crc32",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "crc32", x@jc)
            column(jc)
          })

#' hash
#'
#' Calculates the hash code of given columns, and returns the result as a int column.
#'
#' @rdname hash
#' @name hash
#' @family misc_funcs
#' @export
#' @examples \dontrun{hash(df$c)}
setMethod("hash",
          signature(x = "Column"),
          function(x, ...) {
            jcols <- lapply(list(x, ...), function (x) {
              stopifnot(class(x) == "Column")
              x@jc
            })
            jc <- callJStatic("org.apache.spark.sql.functions", "hash", jcols)
            column(jc)
          })

#' dayofmonth
#'
#' Extracts the day of the month as an integer from a given date/timestamp/string.
#'
#' @rdname dayofmonth
#' @name dayofmonth
#' @family datetime_funcs
#' @export
#' @examples \dontrun{dayofmonth(df$c)}
setMethod("dayofmonth",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "dayofmonth", x@jc)
            column(jc)
          })

#' dayofyear
#'
#' Extracts the day of the year as an integer from a given date/timestamp/string.
#'
#' @rdname dayofyear
#' @name dayofyear
#' @family datetime_funcs
#' @export
#' @examples \dontrun{dayofyear(df$c)}
setMethod("dayofyear",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "dayofyear", x@jc)
            column(jc)
          })

#' decode
#'
#' Computes the first argument into a string from a binary using the provided character set
#' (one of 'US-ASCII', 'ISO-8859-1', 'UTF-8', 'UTF-16BE', 'UTF-16LE', 'UTF-16').
#'
#' @rdname decode
#' @name decode
#' @family string_funcs
#' @export
#' @examples \dontrun{decode(df$c, "UTF-8")}
setMethod("decode",
          signature(x = "Column", charset = "character"),
          function(x, charset) {
            jc <- callJStatic("org.apache.spark.sql.functions", "decode", x@jc, charset)
            column(jc)
          })

#' encode
#'
#' Computes the first argument into a binary from a string using the provided character set
#' (one of 'US-ASCII', 'ISO-8859-1', 'UTF-8', 'UTF-16BE', 'UTF-16LE', 'UTF-16').
#'
#' @rdname encode
#' @name encode
#' @family string_funcs
#' @export
#' @examples \dontrun{encode(df$c, "UTF-8")}
setMethod("encode",
          signature(x = "Column", charset = "character"),
          function(x, charset) {
            jc <- callJStatic("org.apache.spark.sql.functions", "encode", x@jc, charset)
            column(jc)
          })

#' exp
#'
#' Computes the exponential of the given value.
#'
#' @rdname exp
#' @name exp
#' @family math_funcs
#' @export
#' @examples \dontrun{exp(df$c)}
setMethod("exp",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "exp", x@jc)
            column(jc)
          })

#' expm1
#'
#' Computes the exponential of the given value minus one.
#'
#' @rdname expm1
#' @name expm1
#' @family math_funcs
#' @export
#' @examples \dontrun{expm1(df$c)}
setMethod("expm1",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "expm1", x@jc)
            column(jc)
          })

#' factorial
#'
#' Computes the factorial of the given value.
#'
#' @rdname factorial
#' @name factorial
#' @family math_funcs
#' @export
#' @examples \dontrun{factorial(df$c)}
setMethod("factorial",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "factorial", x@jc)
            column(jc)
          })

#' first
#'
#' Aggregate function: returns the first value in a group.
#'
#' The function by default returns the first values it sees. It will return the first non-missing
#' value it sees when na.rm is set to true. If all values are missing, then NA is returned.
#'
#' @rdname first
#' @name first
#' @family agg_funcs
#' @export
#' @examples
#' \dontrun{
#' first(df$c)
#' first(df$c, TRUE)
#' }
setMethod("first",
          signature(x = "characterOrColumn"),
          function(x, na.rm = FALSE) {
            col <- if (class(x) == "Column") {
              x@jc
            } else {
              x
            }
            jc <- callJStatic("org.apache.spark.sql.functions", "first", col, na.rm)
            column(jc)
          })

#' floor
#'
#' Computes the floor of the given value.
#'
#' @rdname floor
#' @name floor
#' @family math_funcs
#' @export
#' @examples \dontrun{floor(df$c)}
setMethod("floor",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "floor", x@jc)
            column(jc)
          })

#' hex
#'
#' Computes hex value of the given column.
#'
#' @rdname hex
#' @name hex
#' @family math_funcs
#' @export
#' @examples \dontrun{hex(df$c)}
setMethod("hex",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "hex", x@jc)
            column(jc)
          })

#' hour
#'
#' Extracts the hours as an integer from a given date/timestamp/string.
#'
#' @rdname hour
#' @name hour
#' @family datetime_funcs
#' @export
#' @examples \dontrun{hour(df$c)}
setMethod("hour",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "hour", x@jc)
            column(jc)
          })

#' initcap
#'
#' Returns a new string column by converting the first letter of each word to uppercase.
#' Words are delimited by whitespace.
#'
#' For example, "hello world" will become "Hello World".
#'
#' @rdname initcap
#' @name initcap
#' @family string_funcs
#' @export
#' @examples \dontrun{initcap(df$c)}
setMethod("initcap",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "initcap", x@jc)
            column(jc)
          })

#' is.nan
#'
#' Return true if the column is NaN, alias for \link{isnan}
#'
#' @rdname is.nan
#' @name is.nan
#' @family normal_funcs
#' @export
#' @examples
#' \dontrun{
#' is.nan(df$c)
#' isnan(df$c)
#' }
setMethod("is.nan",
          signature(x = "Column"),
          function(x) {
            isnan(x)
          })

#' @rdname is.nan
#' @name isnan
setMethod("isnan",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "isnan", x@jc)
            column(jc)
          })

#' kurtosis
#'
#' Aggregate function: returns the kurtosis of the values in a group.
#'
#' @rdname kurtosis
#' @name kurtosis
#' @family agg_funcs
#' @export
#' @examples \dontrun{kurtosis(df$c)}
setMethod("kurtosis",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "kurtosis", x@jc)
            column(jc)
          })

#' last
#'
#' Aggregate function: returns the last value in a group.
#'
#' The function by default returns the last values it sees. It will return the last non-missing
#' value it sees when na.rm is set to true. If all values are missing, then NA is returned.
#'
#' @rdname last
#' @name last
#' @family agg_funcs
#' @export
#' @examples
#' \dontrun{
#' last(df$c)
#' last(df$c, TRUE)
#' }
setMethod("last",
          signature(x = "characterOrColumn"),
          function(x, na.rm = FALSE) {
            col <- if (class(x) == "Column") {
              x@jc
            } else {
              x
            }
            jc <- callJStatic("org.apache.spark.sql.functions", "last", col, na.rm)
            column(jc)
          })

#' last_day
#'
#' Given a date column, returns the last day of the month which the given date belongs to.
#' For example, input "2015-07-27" returns "2015-07-31" since July 31 is the last day of the
#' month in July 2015.
#'
#' @rdname last_day
#' @name last_day
#' @family datetime_funcs
#' @export
#' @examples \dontrun{last_day(df$c)}
setMethod("last_day",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "last_day", x@jc)
            column(jc)
          })

#' length
#'
#' Computes the length of a given string or binary column.
#'
#' @rdname length
#' @name length
#' @family string_funcs
#' @export
#' @examples \dontrun{length(df$c)}
setMethod("length",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "length", x@jc)
            column(jc)
          })

#' log
#'
#' Computes the natural logarithm of the given value.
#'
#' @rdname log
#' @name log
#' @family math_funcs
#' @export
#' @examples \dontrun{log(df$c)}
setMethod("log",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "log", x@jc)
            column(jc)
          })

#' log10
#'
#' Computes the logarithm of the given value in base 10.
#'
#' @rdname log10
#' @name log10
#' @family math_funcs
#' @export
#' @examples \dontrun{log10(df$c)}
setMethod("log10",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "log10", x@jc)
            column(jc)
          })

#' log1p
#'
#' Computes the natural logarithm of the given value plus one.
#'
#' @rdname log1p
#' @name log1p
#' @family math_funcs
#' @export
#' @examples \dontrun{log1p(df$c)}
setMethod("log1p",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "log1p", x@jc)
            column(jc)
          })

#' log2
#'
#' Computes the logarithm of the given column in base 2.
#'
#' @rdname log2
#' @name log2
#' @family math_funcs
#' @export
#' @examples \dontrun{log2(df$c)}
setMethod("log2",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "log2", x@jc)
            column(jc)
          })

#' lower
#'
#' Converts a string column to lower case.
#'
#' @rdname lower
#' @name lower
#' @family string_funcs
#' @export
#' @examples \dontrun{lower(df$c)}
setMethod("lower",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "lower", x@jc)
            column(jc)
          })

#' ltrim
#'
#' Trim the spaces from left end for the specified string value.
#'
#' @rdname ltrim
#' @name ltrim
#' @family string_funcs
#' @export
#' @examples \dontrun{ltrim(df$c)}
setMethod("ltrim",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "ltrim", x@jc)
            column(jc)
          })

#' max
#'
#' Aggregate function: returns the maximum value of the expression in a group.
#'
#' @rdname max
#' @name max
#' @family agg_funcs
#' @export
#' @examples \dontrun{max(df$c)}
setMethod("max",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "max", x@jc)
            column(jc)
          })

#' md5
#'
#' Calculates the MD5 digest of a binary column and returns the value
#' as a 32 character hex string.
#'
#' @rdname md5
#' @name md5
#' @family misc_funcs
#' @export
#' @examples \dontrun{md5(df$c)}
setMethod("md5",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "md5", x@jc)
            column(jc)
          })

#' mean
#'
#' Aggregate function: returns the average of the values in a group.
#' Alias for avg.
#'
#' @rdname mean
#' @name mean
#' @family agg_funcs
#' @export
#' @examples \dontrun{mean(df$c)}
setMethod("mean",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "mean", x@jc)
            column(jc)
          })

#' min
#'
#' Aggregate function: returns the minimum value of the expression in a group.
#'
#' @rdname min
#' @name min
#' @family agg_funcs
#' @export
#' @examples \dontrun{min(df$c)}
setMethod("min",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "min", x@jc)
            column(jc)
          })

#' minute
#'
#' Extracts the minutes as an integer from a given date/timestamp/string.
#'
#' @rdname minute
#' @name minute
#' @family datetime_funcs
#' @export
#' @examples \dontrun{minute(df$c)}
setMethod("minute",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "minute", x@jc)
            column(jc)
          })

#' month
#'
#' Extracts the month as an integer from a given date/timestamp/string.
#'
#' @rdname month
#' @name month
#' @family datetime_funcs
#' @export
#' @examples \dontrun{month(df$c)}
setMethod("month",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "month", x@jc)
            column(jc)
          })

#' negate
#'
#' Unary minus, i.e. negate the expression.
#'
#' @rdname negate
#' @name negate
#' @family normal_funcs
#' @export
#' @examples \dontrun{negate(df$c)}
setMethod("negate",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "negate", x@jc)
            column(jc)
          })

#' quarter
#'
#' Extracts the quarter as an integer from a given date/timestamp/string.
#'
#' @rdname quarter
#' @name quarter
#' @family datetime_funcs
#' @export
#' @examples \dontrun{quarter(df$c)}
setMethod("quarter",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "quarter", x@jc)
            column(jc)
          })

#' reverse
#'
#' Reverses the string column and returns it as a new string column.
#'
#' @rdname reverse
#' @name reverse
#' @family string_funcs
#' @export
#' @examples \dontrun{reverse(df$c)}
setMethod("reverse",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "reverse", x@jc)
            column(jc)
          })

#' rint
#'
#' Returns the double value that is closest in value to the argument and
#' is equal to a mathematical integer.
#'
#' @rdname rint
#' @name rint
#' @family math_funcs
#' @export
#' @examples \dontrun{rint(df$c)}
setMethod("rint",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "rint", x@jc)
            column(jc)
          })

#' round
#'
#' Returns the value of the column `e` rounded to 0 decimal places.
#'
#' @rdname round
#' @name round
#' @family math_funcs
#' @export
#' @examples \dontrun{round(df$c)}
setMethod("round",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "round", x@jc)
            column(jc)
          })

#' rtrim
#'
#' Trim the spaces from right end for the specified string value.
#'
#' @rdname rtrim
#' @name rtrim
#' @family string_funcs
#' @export
#' @examples \dontrun{rtrim(df$c)}
setMethod("rtrim",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "rtrim", x@jc)
            column(jc)
          })

#' sd
#'
#' Aggregate function: alias for \link{stddev_samp}
#'
#' @rdname sd
#' @name sd
#' @family agg_funcs
#' @seealso \link{stddev_pop}, \link{stddev_samp}
#' @export
#' @examples
#'\dontrun{
#'stddev(df$c)
#'select(df, stddev(df$age))
#'agg(df, sd(df$age))
#'}
setMethod("sd",
          signature(x = "Column"),
          function(x) {
            # In R, sample standard deviation is calculated with the sd() function.
            stddev_samp(x)
          })

#' second
#'
#' Extracts the seconds as an integer from a given date/timestamp/string.
#'
#' @rdname second
#' @name second
#' @family datetime_funcs
#' @export
#' @examples \dontrun{second(df$c)}
setMethod("second",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "second", x@jc)
            column(jc)
          })

#' sha1
#'
#' Calculates the SHA-1 digest of a binary column and returns the value
#' as a 40 character hex string.
#'
#' @rdname sha1
#' @name sha1
#' @family misc_funcs
#' @export
#' @examples \dontrun{sha1(df$c)}
setMethod("sha1",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "sha1", x@jc)
            column(jc)
          })

#' signum
#'
#' Computes the signum of the given value.
#'
#' @rdname signum
#' @name signum
#' @family math_funcs
#' @export
#' @examples \dontrun{signum(df$c)}
setMethod("signum",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "signum", x@jc)
            column(jc)
          })

#' sin
#'
#' Computes the sine of the given value.
#'
#' @rdname sin
#' @name sin
#' @family math_funcs
#' @export
#' @examples \dontrun{sin(df$c)}
setMethod("sin",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "sin", x@jc)
            column(jc)
          })

#' sinh
#'
#' Computes the hyperbolic sine of the given value.
#'
#' @rdname sinh
#' @name sinh
#' @family math_funcs
#' @export
#' @examples \dontrun{sinh(df$c)}
setMethod("sinh",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "sinh", x@jc)
            column(jc)
          })

#' skewness
#'
#' Aggregate function: returns the skewness of the values in a group.
#'
#' @rdname skewness
#' @name skewness
#' @family agg_funcs
#' @export
#' @examples \dontrun{skewness(df$c)}
setMethod("skewness",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "skewness", x@jc)
            column(jc)
          })

#' soundex
#'
#' Return the soundex code for the specified expression.
#'
#' @rdname soundex
#' @name soundex
#' @family string_funcs
#' @export
#' @examples \dontrun{soundex(df$c)}
setMethod("soundex",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "soundex", x@jc)
            column(jc)
          })

#' @rdname sd
#' @name stddev
setMethod("stddev",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "stddev", x@jc)
            column(jc)
          })

#' stddev_pop
#'
#' Aggregate function: returns the population standard deviation of the expression in a group.
#'
#' @rdname stddev_pop
#' @name stddev_pop
#' @family agg_funcs
#' @seealso \link{sd}, \link{stddev_samp}
#' @export
#' @examples \dontrun{stddev_pop(df$c)}
setMethod("stddev_pop",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "stddev_pop", x@jc)
            column(jc)
          })

#' stddev_samp
#'
#' Aggregate function: returns the unbiased sample standard deviation of the expression in a group.
#'
#' @rdname stddev_samp
#' @name stddev_samp
#' @family agg_funcs
#' @seealso \link{stddev_pop}, \link{sd}
#' @export
#' @examples \dontrun{stddev_samp(df$c)}
setMethod("stddev_samp",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "stddev_samp", x@jc)
            column(jc)
          })

#' struct
#'
#' Creates a new struct column that composes multiple input columns.
#'
#' @rdname struct
#' @name struct
#' @family normal_funcs
#' @export
#' @examples
#' \dontrun{
#' struct(df$c, df$d)
#' struct("col1", "col2")
#' }
setMethod("struct",
          signature(x = "characterOrColumn"),
          function(x, ...) {
            if (class(x) == "Column") {
              jcols <- lapply(list(x, ...), function(x) { x@jc })
              jc <- callJStatic("org.apache.spark.sql.functions", "struct", jcols)
            } else {
              jc <- callJStatic("org.apache.spark.sql.functions", "struct", x, list(...))
            }
            column(jc)
          })

#' sqrt
#'
#' Computes the square root of the specified float value.
#'
#' @rdname sqrt
#' @name sqrt
#' @family math_funcs
#' @export
#' @examples \dontrun{sqrt(df$c)}
setMethod("sqrt",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "sqrt", x@jc)
            column(jc)
          })

#' sum
#'
#' Aggregate function: returns the sum of all values in the expression.
#'
#' @rdname sum
#' @name sum
#' @family agg_funcs
#' @export
#' @examples \dontrun{sum(df$c)}
setMethod("sum",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "sum", x@jc)
            column(jc)
          })

#' sumDistinct
#'
#' Aggregate function: returns the sum of distinct values in the expression.
#'
#' @rdname sumDistinct
#' @name sumDistinct
#' @family agg_funcs
#' @export
#' @examples \dontrun{sumDistinct(df$c)}
setMethod("sumDistinct",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "sumDistinct", x@jc)
            column(jc)
          })

#' tan
#'
#' Computes the tangent of the given value.
#'
#' @rdname tan
#' @name tan
#' @family math_funcs
#' @export
#' @examples \dontrun{tan(df$c)}
setMethod("tan",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "tan", x@jc)
            column(jc)
          })

#' tanh
#'
#' Computes the hyperbolic tangent of the given value.
#'
#' @rdname tanh
#' @name tanh
#' @family math_funcs
#' @export
#' @examples \dontrun{tanh(df$c)}
setMethod("tanh",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "tanh", x@jc)
            column(jc)
          })

#' toDegrees
#'
#' Converts an angle measured in radians to an approximately equivalent angle measured in degrees.
#'
#' @rdname toDegrees
#' @name toDegrees
#' @family math_funcs
#' @export
#' @examples \dontrun{toDegrees(df$c)}
setMethod("toDegrees",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "toDegrees", x@jc)
            column(jc)
          })

#' toRadians
#'
#' Converts an angle measured in degrees to an approximately equivalent angle measured in radians.
#'
#' @rdname toRadians
#' @name toRadians
#' @family math_funcs
#' @export
#' @examples \dontrun{toRadians(df$c)}
setMethod("toRadians",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "toRadians", x@jc)
            column(jc)
          })

#' to_date
#'
#' Converts the column into DateType.
#'
#' @rdname to_date
#' @name to_date
#' @family datetime_funcs
#' @export
#' @examples \dontrun{to_date(df$c)}
setMethod("to_date",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "to_date", x@jc)
            column(jc)
          })

#' trim
#'
#' Trim the spaces from both ends for the specified string column.
#'
#' @rdname trim
#' @name trim
#' @family string_funcs
#' @export
#' @examples \dontrun{trim(df$c)}
setMethod("trim",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "trim", x@jc)
            column(jc)
          })

#' unbase64
#'
#' Decodes a BASE64 encoded string column and returns it as a binary column.
#' This is the reverse of base64.
#'
#' @rdname unbase64
#' @name unbase64
#' @family string_funcs
#' @export
#' @examples \dontrun{unbase64(df$c)}
setMethod("unbase64",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "unbase64", x@jc)
            column(jc)
          })

#' unhex
#'
#' Inverse of hex. Interprets each pair of characters as a hexadecimal number
#' and converts to the byte representation of number.
#'
#' @rdname unhex
#' @name unhex
#' @family math_funcs
#' @export
#' @examples \dontrun{unhex(df$c)}
setMethod("unhex",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "unhex", x@jc)
            column(jc)
          })

#' upper
#'
#' Converts a string column to upper case.
#'
#' @rdname upper
#' @name upper
#' @family string_funcs
#' @export
#' @examples \dontrun{upper(df$c)}
setMethod("upper",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "upper", x@jc)
            column(jc)
          })

#' var
#'
#' Aggregate function: alias for \link{var_samp}.
#'
#' @rdname var
#' @name var
#' @family agg_funcs
#' @seealso \link{var_pop}, \link{var_samp}
#' @export
#' @examples
#'\dontrun{
#'variance(df$c)
#'select(df, var_pop(df$age))
#'agg(df, var(df$age))
#'}
setMethod("var",
          signature(x = "Column"),
          function(x) {
            # In R, sample variance is calculated with the var() function.
            var_samp(x)
          })

#' @rdname var
#' @name variance
setMethod("variance",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "variance", x@jc)
            column(jc)
          })

#' var_pop
#'
#' Aggregate function: returns the population variance of the values in a group.
#'
#' @rdname var_pop
#' @name var_pop
#' @family agg_funcs
#' @seealso \link{var}, \link{var_samp}
#' @export
#' @examples \dontrun{var_pop(df$c)}
setMethod("var_pop",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "var_pop", x@jc)
            column(jc)
          })

#' var_samp
#'
#' Aggregate function: returns the unbiased variance of the values in a group.
#'
#' @rdname var_samp
#' @name var_samp
#' @family agg_funcs
#' @seealso \link{var_pop}, \link{var}
#' @export
#' @examples \dontrun{var_samp(df$c)}
setMethod("var_samp",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "var_samp", x@jc)
            column(jc)
          })

#' weekofyear
#'
#' Extracts the week number as an integer from a given date/timestamp/string.
#'
#' @rdname weekofyear
#' @name weekofyear
#' @family datetime_funcs
#' @export
#' @examples \dontrun{weekofyear(df$c)}
setMethod("weekofyear",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "weekofyear", x@jc)
            column(jc)
          })

#' year
#'
#' Extracts the year as an integer from a given date/timestamp/string.
#'
#' @rdname year
#' @name year
#' @family datetime_funcs
#' @export
#' @examples \dontrun{year(df$c)}
setMethod("year",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "year", x@jc)
            column(jc)
          })

#' atan2
#'
#' Returns the angle theta from the conversion of rectangular coordinates (x, y) to
#' polar coordinates (r, theta).
#'
#' @rdname atan2
#' @name atan2
#' @family math_funcs
#' @export
#' @examples \dontrun{atan2(df$c, x)}
setMethod("atan2", signature(y = "Column"),
          function(y, x) {
            if (class(x) == "Column") {
              x <- x@jc
            }
            jc <- callJStatic("org.apache.spark.sql.functions", "atan2", y@jc, x)
            column(jc)
          })

#' datediff
#'
#' Returns the number of days from `start` to `end`.
#'
#' @rdname datediff
#' @name datediff
#' @family datetime_funcs
#' @export
#' @examples \dontrun{datediff(df$c, x)}
setMethod("datediff", signature(y = "Column"),
          function(y, x) {
            if (class(x) == "Column") {
              x <- x@jc
            }
            jc <- callJStatic("org.apache.spark.sql.functions", "datediff", y@jc, x)
            column(jc)
          })

#' hypot
#'
#' Computes `sqrt(a^2^ + b^2^)` without intermediate overflow or underflow.
#'
#' @rdname hypot
#' @name hypot
#' @family math_funcs
#' @export
#' @examples \dontrun{hypot(df$c, x)}
setMethod("hypot", signature(y = "Column"),
          function(y, x) {
            if (class(x) == "Column") {
              x <- x@jc
            }
            jc <- callJStatic("org.apache.spark.sql.functions", "hypot", y@jc, x)
            column(jc)
          })

#' levenshtein
#'
#' Computes the Levenshtein distance of the two given string columns.
#'
#' @rdname levenshtein
#' @name levenshtein
#' @family string_funcs
#' @export
#' @examples \dontrun{levenshtein(df$c, x)}
setMethod("levenshtein", signature(y = "Column"),
          function(y, x) {
            if (class(x) == "Column") {
              x <- x@jc
            }
            jc <- callJStatic("org.apache.spark.sql.functions", "levenshtein", y@jc, x)
            column(jc)
          })

#' months_between
#'
#' Returns number of months between dates `date1` and `date2`.
#'
#' @rdname months_between
#' @name months_between
#' @family datetime_funcs
#' @export
#' @examples \dontrun{months_between(df$c, x)}
setMethod("months_between", signature(y = "Column"),
          function(y, x) {
            if (class(x) == "Column") {
              x <- x@jc
            }
            jc <- callJStatic("org.apache.spark.sql.functions", "months_between", y@jc, x)
            column(jc)
          })

#' nanvl
#'
#' Returns col1 if it is not NaN, or col2 if col1 is NaN.
#' hhBoth inputs should be floating point columns (DoubleType or FloatType).
#'
#' @rdname nanvl
#' @name nanvl
#' @family normal_funcs
#' @export
#' @examples \dontrun{nanvl(df$c, x)}
setMethod("nanvl", signature(y = "Column"),
          function(y, x) {
            if (class(x) == "Column") {
              x <- x@jc
            }
            jc <- callJStatic("org.apache.spark.sql.functions", "nanvl", y@jc, x)
            column(jc)
          })

#' pmod
#'
#' Returns the positive value of dividend mod divisor.
#'
#' @rdname pmod
#' @name pmod
#' @docType methods
#' @family math_funcs
#' @export
#' @examples \dontrun{pmod(df$c, x)}
setMethod("pmod", signature(y = "Column"),
          function(y, x) {
            if (class(x) == "Column") {
              x <- x@jc
            }
            jc <- callJStatic("org.apache.spark.sql.functions", "pmod", y@jc, x)
            column(jc)
          })


#' Approx Count Distinct
#'
#' @family agg_funcs
#' @rdname approxCountDistinct
#' @name approxCountDistinct
#' @return the approximate number of distinct items in a group.
#' @export
#' @examples \dontrun{approxCountDistinct(df$c, 0.02)}
setMethod("approxCountDistinct",
          signature(x = "Column"),
          function(x, rsd = 0.05) {
            jc <- callJStatic("org.apache.spark.sql.functions", "approxCountDistinct", x@jc, rsd)
            column(jc)
          })

#' Count Distinct
#'
#' @family agg_funcs
#' @rdname countDistinct
#' @name countDistinct
#' @return the number of distinct items in a group.
#' @export
#' @examples \dontrun{countDistinct(df$c)}
setMethod("countDistinct",
          signature(x = "Column"),
          function(x, ...) {
            jcols <- lapply(list(...), function (x) {
              stopifnot(class(x) == "Column")
              x@jc
            })
            jc <- callJStatic("org.apache.spark.sql.functions", "countDistinct", x@jc,
                              jcols)
            column(jc)
          })


#' concat
#'
#' Concatenates multiple input string columns together into a single string column.
#'
#' @family string_funcs
#' @rdname concat
#' @name concat
#' @export
#' @examples \dontrun{concat(df$strings, df$strings2)}
setMethod("concat",
          signature(x = "Column"),
          function(x, ...) {
            jcols <- lapply(list(x, ...), function (x) {
              stopifnot(class(x) == "Column")
              x@jc
            })
            jc <- callJStatic("org.apache.spark.sql.functions", "concat", jcols)
            column(jc)
          })

#' greatest
#'
#' Returns the greatest value of the list of column names, skipping null values.
#' This function takes at least 2 parameters. It will return null if all parameters are null.
#'
#' @family normal_funcs
#' @rdname greatest
#' @name greatest
#' @export
#' @examples \dontrun{greatest(df$c, df$d)}
setMethod("greatest",
          signature(x = "Column"),
          function(x, ...) {
            stopifnot(length(list(...)) > 0)
            jcols <- lapply(list(x, ...), function (x) {
              stopifnot(class(x) == "Column")
              x@jc
            })
            jc <- callJStatic("org.apache.spark.sql.functions", "greatest", jcols)
            column(jc)
          })

#' least
#'
#' Returns the least value of the list of column names, skipping null values.
#' This function takes at least 2 parameters. It will return null if all parameters are null.
#'
#' @family normal_funcs
#' @rdname least
#' @name least
#' @export
#' @examples \dontrun{least(df$c, df$d)}
setMethod("least",
          signature(x = "Column"),
          function(x, ...) {
            stopifnot(length(list(...)) > 0)
            jcols <- lapply(list(x, ...), function (x) {
              stopifnot(class(x) == "Column")
              x@jc
            })
            jc <- callJStatic("org.apache.spark.sql.functions", "least", jcols)
            column(jc)
          })

#' ceiling
#'
#' Computes the ceiling of the given value.
#'
#' @rdname ceil
#' @name ceiling
#' @export
#' @examples \dontrun{ceiling(df$c)}
setMethod("ceiling",
          signature(x = "Column"),
          function(x) {
            ceil(x)
          })

#' sign
#'
#' Computes the signum of the given value.
#'
#' @rdname signum
#' @name sign
#' @export
#' @examples \dontrun{sign(df$c)}
setMethod("sign", signature(x = "Column"),
          function(x) {
            signum(x)
          })

#' n_distinct
#'
#' Aggregate function: returns the number of distinct items in a group.
#'
#' @rdname countDistinct
#' @name n_distinct
#' @export
#' @examples \dontrun{n_distinct(df$c)}
setMethod("n_distinct", signature(x = "Column"),
          function(x, ...) {
            countDistinct(x, ...)
          })

#' n
#'
#' Aggregate function: returns the number of items in a group.
#'
#' @rdname count
#' @name n
#' @export
#' @examples \dontrun{n(df$c)}
setMethod("n", signature(x = "Column"),
          function(x) {
            count(x)
          })

#' date_format
#'
#' Converts a date/timestamp/string to a value of string in the format specified by the date
#' format given by the second argument.
#'
#' A pattern could be for instance \preformatted{dd.MM.yyyy} and could return a string like '18.03.1993'. All
#' pattern letters of \code{java.text.SimpleDateFormat} can be used.
#'
#' NOTE: Use when ever possible specialized functions like \code{year}. These benefit from a
#' specialized implementation.
#'
#' @family datetime_funcs
#' @rdname date_format
#' @name date_format
#' @export
#' @examples \dontrun{date_format(df$t, 'MM/dd/yyy')}
setMethod("date_format", signature(y = "Column", x = "character"),
          function(y, x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "date_format", y@jc, x)
            column(jc)
          })

#' from_utc_timestamp
#'
#' Assumes given timestamp is UTC and converts to given timezone.
#'
#' @family datetime_funcs
#' @rdname from_utc_timestamp
#' @name from_utc_timestamp
#' @export
#' @examples \dontrun{from_utc_timestamp(df$t, 'PST')}
setMethod("from_utc_timestamp", signature(y = "Column", x = "character"),
          function(y, x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "from_utc_timestamp", y@jc, x)
            column(jc)
          })

#' instr
#'
#' Locate the position of the first occurrence of substr column in the given string.
#' Returns null if either of the arguments are null.
#'
#' NOTE: The position is not zero based, but 1 based index, returns 0 if substr
#' could not be found in str.
#'
#' @family string_funcs
#' @rdname instr
#' @name instr
#' @export
#' @examples \dontrun{instr(df$c, 'b')}
setMethod("instr", signature(y = "Column", x = "character"),
          function(y, x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "instr", y@jc, x)
            column(jc)
          })

#' next_day
#'
#' Given a date column, returns the first date which is later than the value of the date column
#' that is on the specified day of the week.
#'
#' For example, \code{next_day('2015-07-27', "Sunday")} returns 2015-08-02 because that is the first
#' Sunday after 2015-07-27.
#'
#' Day of the week parameter is case insensitive, and accepts first three or two characters:
#' "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun".
#'
#' @family datetime_funcs
#' @rdname next_day
#' @name next_day
#' @export
#' @examples
#'\dontrun{
#'next_day(df$d, 'Sun')
#'next_day(df$d, 'Sunday')
#'}
setMethod("next_day", signature(y = "Column", x = "character"),
          function(y, x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "next_day", y@jc, x)
            column(jc)
          })

#' to_utc_timestamp
#'
#' Assumes given timestamp is in given timezone and converts to UTC.
#'
#' @family datetime_funcs
#' @rdname to_utc_timestamp
#' @name to_utc_timestamp
#' @export
#' @examples \dontrun{to_utc_timestamp(df$t, 'PST')}
setMethod("to_utc_timestamp", signature(y = "Column", x = "character"),
          function(y, x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "to_utc_timestamp", y@jc, x)
            column(jc)
          })

#' add_months
#'
#' Returns the date that is numMonths after startDate.
#'
#' @name add_months
#' @family datetime_funcs
#' @rdname add_months
#' @export
#' @examples \dontrun{add_months(df$d, 1)}
setMethod("add_months", signature(y = "Column", x = "numeric"),
          function(y, x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "add_months", y@jc, as.integer(x))
            column(jc)
          })

#' date_add
#'
#' Returns the date that is `days` days after `start`
#'
#' @family datetime_funcs
#' @rdname date_add
#' @name date_add
#' @export
#' @examples \dontrun{date_add(df$d, 1)}
setMethod("date_add", signature(y = "Column", x = "numeric"),
          function(y, x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "date_add", y@jc, as.integer(x))
            column(jc)
          })

#' date_sub
#'
#' Returns the date that is `days` days before `start`
#'
#' @family datetime_funcs
#' @rdname date_sub
#' @name date_sub
#' @export
#' @examples \dontrun{date_sub(df$d, 1)}
setMethod("date_sub", signature(y = "Column", x = "numeric"),
          function(y, x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "date_sub", y@jc, as.integer(x))
            column(jc)
          })

#' format_number
#'
#' Formats numeric column y to a format like '#,###,###.##', rounded to x decimal places,
#' and returns the result as a string column.
#'
#' If x is 0, the result has no decimal point or fractional part.
#' If x < 0, the result will be null.
#'
#' @param y column to format
#' @param x number of decimal place to format to
#' @family string_funcs
#' @rdname format_number
#' @name format_number
#' @export
#' @examples \dontrun{format_number(df$n, 4)}
setMethod("format_number", signature(y = "Column", x = "numeric"),
          function(y, x) {
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "format_number",
                              y@jc, as.integer(x))
            column(jc)
          })

#' sha2
#'
#' Calculates the SHA-2 family of hash functions of a binary column and
#' returns the value as a hex string.
#'
#' @param y column to compute SHA-2 on.
#' @param x one of 224, 256, 384, or 512.
#' @family misc_funcs
#' @rdname sha2
#' @name sha2
#' @export
#' @examples \dontrun{sha2(df$c, 256)}
setMethod("sha2", signature(y = "Column", x = "numeric"),
          function(y, x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "sha2", y@jc, as.integer(x))
            column(jc)
          })

#' shiftLeft
#'
#' Shift the given value numBits left. If the given value is a long value, this function
#' will return a long value else it will return an integer value.
#'
#' @family math_funcs
#' @rdname shiftLeft
#' @name shiftLeft
#' @export
#' @examples \dontrun{shiftLeft(df$c, 1)}
setMethod("shiftLeft", signature(y = "Column", x = "numeric"),
          function(y, x) {
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "shiftLeft",
                              y@jc, as.integer(x))
            column(jc)
          })

#' shiftRight
#'
#' Shift the given value numBits right. If the given value is a long value, it will return
#' a long value else it will return an integer value.
#'
#' @family math_funcs
#' @rdname shiftRight
#' @name shiftRight
#' @export
#' @examples \dontrun{shiftRight(df$c, 1)}
setMethod("shiftRight", signature(y = "Column", x = "numeric"),
          function(y, x) {
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "shiftRight",
                              y@jc, as.integer(x))
            column(jc)
          })

#' shiftRightUnsigned
#'
#' Unsigned shift the given value numBits right. If the given value is a long value,
#' it will return a long value else it will return an integer value.
#'
#' @family math_funcs
#' @rdname shiftRightUnsigned
#' @name shiftRightUnsigned
#' @export
#' @examples \dontrun{shiftRightUnsigned(df$c, 1)}
setMethod("shiftRightUnsigned", signature(y = "Column", x = "numeric"),
          function(y, x) {
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "shiftRightUnsigned",
                              y@jc, as.integer(x))
            column(jc)
          })

#' concat_ws
#'
#' Concatenates multiple input string columns together into a single string column,
#' using the given separator.
#'
#' @family string_funcs
#' @rdname concat_ws
#' @name concat_ws
#' @export
#' @examples \dontrun{concat_ws('-', df$s, df$d)}
setMethod("concat_ws", signature(sep = "character", x = "Column"),
          function(sep, x, ...) {
            jcols <- lapply(list(x, ...), function(x) { x@jc })
            jc <- callJStatic("org.apache.spark.sql.functions", "concat_ws", sep, jcols)
            column(jc)
          })

#' conv
#'
#' Convert a number in a string column from one base to another.
#'
#' @family math_funcs
#' @rdname conv
#' @name conv
#' @export
#' @examples \dontrun{conv(df$n, 2, 16)}
setMethod("conv", signature(x = "Column", fromBase = "numeric", toBase = "numeric"),
          function(x, fromBase, toBase) {
            fromBase <- as.integer(fromBase)
            toBase <- as.integer(toBase)
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "conv",
                              x@jc, fromBase, toBase)
            column(jc)
          })

#' expr
#'
#' Parses the expression string into the column that it represents, similar to
#' DataFrame.selectExpr
#'
#' @family normal_funcs
#' @rdname expr
#' @name expr
#' @export
#' @examples \dontrun{expr('length(name)')}
setMethod("expr", signature(x = "character"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "expr", x)
            column(jc)
          })

#' format_string
#'
#' Formats the arguments in printf-style and returns the result as a string column.
#'
#' @family string_funcs
#' @rdname format_string
#' @name format_string
#' @export
#' @examples \dontrun{format_string('%d %s', df$a, df$b)}
setMethod("format_string", signature(format = "character", x = "Column"),
          function(format, x, ...) {
            jcols <- lapply(list(x, ...), function(arg) { arg@jc })
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "format_string",
                              format, jcols)
            column(jc)
          })

#' from_unixtime
#'
#' Converts the number of seconds from unix epoch (1970-01-01 00:00:00 UTC) to a string
#' representing the timestamp of that moment in the current system time zone in the given
#' format.
#'
#' @family datetime_funcs
#' @rdname from_unixtime
#' @name from_unixtime
#' @export
#' @examples
#'\dontrun{
#'from_unixtime(df$t)
#'from_unixtime(df$t, 'yyyy/MM/dd HH')
#'}
setMethod("from_unixtime", signature(x = "Column"),
          function(x, format = "yyyy-MM-dd HH:mm:ss") {
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "from_unixtime",
                              x@jc, format)
            column(jc)
          })

#' locate
#'
#' Locate the position of the first occurrence of substr.
#' NOTE: The position is not zero based, but 1 based index, returns 0 if substr
#' could not be found in str.
#'
#' @family string_funcs
#' @rdname locate
#' @name locate
#' @export
#' @examples \dontrun{locate('b', df$c, 1)}
setMethod("locate", signature(substr = "character", str = "Column"),
          function(substr, str, pos = 0) {
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "locate",
                              substr, str@jc, as.integer(pos))
            column(jc)
          })

#' lpad
#'
#' Left-pad the string column with
#'
#' @family string_funcs
#' @rdname lpad
#' @name lpad
#' @export
#' @examples \dontrun{lpad(df$c, 6, '#')}
setMethod("lpad", signature(x = "Column", len = "numeric", pad = "character"),
          function(x, len, pad) {
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "lpad",
                              x@jc, as.integer(len), pad)
            column(jc)
          })

#' rand
#'
#' Generate a random column with i.i.d. samples from U[0.0, 1.0].
#'
#' @family normal_funcs
#' @rdname rand
#' @name rand
#' @export
#' @examples \dontrun{rand()}
setMethod("rand", signature(seed = "missing"),
          function(seed) {
            jc <- callJStatic("org.apache.spark.sql.functions", "rand")
            column(jc)
          })

#' @rdname rand
#' @name rand
#' @export
setMethod("rand", signature(seed = "numeric"),
          function(seed) {
            jc <- callJStatic("org.apache.spark.sql.functions", "rand", as.integer(seed))
            column(jc)
          })

#' randn
#'
#' Generate a column with i.i.d. samples from the standard normal distribution.
#'
#' @family normal_funcs
#' @rdname randn
#' @name randn
#' @export
#' @examples \dontrun{randn()}
setMethod("randn", signature(seed = "missing"),
          function(seed) {
            jc <- callJStatic("org.apache.spark.sql.functions", "randn")
            column(jc)
          })

#' @rdname randn
#' @name randn
#' @export
setMethod("randn", signature(seed = "numeric"),
          function(seed) {
            jc <- callJStatic("org.apache.spark.sql.functions", "randn", as.integer(seed))
            column(jc)
          })

#' regexp_extract
#'
#' Extract a specific(idx) group identified by a java regex, from the specified string column.
#'
#' @family string_funcs
#' @rdname regexp_extract
#' @name regexp_extract
#' @export
#' @examples \dontrun{regexp_extract(df$c, '(\d+)-(\d+)', 1)}
setMethod("regexp_extract",
          signature(x = "Column", pattern = "character", idx = "numeric"),
          function(x, pattern, idx) {
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "regexp_extract",
                              x@jc, pattern, as.integer(idx))
            column(jc)
          })

#' regexp_replace
#'
#' Replace all substrings of the specified string value that match regexp with rep.
#'
#' @family string_funcs
#' @rdname regexp_replace
#' @name regexp_replace
#' @export
#' @examples \dontrun{regexp_replace(df$c, '(\\d+)', '--')}
setMethod("regexp_replace",
          signature(x = "Column", pattern = "character", replacement = "character"),
          function(x, pattern, replacement) {
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "regexp_replace",
                              x@jc, pattern, replacement)
            column(jc)
          })

#' rpad
#'
#' Right-padded with pad to a length of len.
#'
#' @family string_funcs
#' @rdname rpad
#' @name rpad
#' @export
#' @examples \dontrun{rpad(df$c, 6, '#')}
setMethod("rpad", signature(x = "Column", len = "numeric", pad = "character"),
          function(x, len, pad) {
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "rpad",
                              x@jc, as.integer(len), pad)
            column(jc)
          })

#' substring_index
#'
#' Returns the substring from string str before count occurrences of the delimiter delim.
#' If count is positive, everything the left of the final delimiter (counting from left) is
#' returned. If count is negative, every to the right of the final delimiter (counting from the
#' right) is returned. substring_index performs a case-sensitive match when searching for delim.
#'
#' @family string_funcs
#' @rdname substring_index
#' @name substring_index
#' @export
#' @examples
#'\dontrun{
#'substring_index(df$c, '.', 2)
#'substring_index(df$c, '.', -1)
#'}
setMethod("substring_index",
          signature(x = "Column", delim = "character", count = "numeric"),
          function(x, delim, count) {
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "substring_index",
                              x@jc, delim, as.integer(count))
            column(jc)
          })

#' translate
#'
#' Translate any character in the src by a character in replaceString.
#' The characters in replaceString is corresponding to the characters in matchingString.
#' The translate will happen when any character in the string matching with the character
#' in the matchingString.
#'
#' @family string_funcs
#' @rdname translate
#' @name translate
#' @export
#' @examples \dontrun{translate(df$c, 'rnlt', '123')}
setMethod("translate",
          signature(x = "Column", matchingString = "character", replaceString = "character"),
          function(x, matchingString, replaceString) {
            jc <- callJStatic("org.apache.spark.sql.functions",
                              "translate", x@jc, matchingString, replaceString)
            column(jc)
          })

#' unix_timestamp
#'
#' Gets current Unix timestamp in seconds.
#'
#' @family datetime_funcs
#' @rdname unix_timestamp
#' @name unix_timestamp
#' @export
#' @examples
#'\dontrun{
#'unix_timestamp()
#'unix_timestamp(df$t)
#'unix_timestamp(df$t, 'yyyy-MM-dd HH')
#'}
setMethod("unix_timestamp", signature(x = "missing", format = "missing"),
          function(x, format) {
            jc <- callJStatic("org.apache.spark.sql.functions", "unix_timestamp")
            column(jc)
          })

#' @rdname unix_timestamp
#' @name unix_timestamp
#' @export
setMethod("unix_timestamp", signature(x = "Column", format = "missing"),
          function(x, format) {
            jc <- callJStatic("org.apache.spark.sql.functions", "unix_timestamp", x@jc)
            column(jc)
          })

#' @rdname unix_timestamp
#' @name unix_timestamp
#' @export
setMethod("unix_timestamp", signature(x = "Column", format = "character"),
          function(x, format = "yyyy-MM-dd HH:mm:ss") {
            jc <- callJStatic("org.apache.spark.sql.functions", "unix_timestamp", x@jc, format)
            column(jc)
          })
#' when
#'
#' Evaluates a list of conditions and returns one of multiple possible result expressions.
#' For unmatched expressions null is returned.
#'
#' @family normal_funcs
#' @rdname when
#' @name when
#' @seealso \link{ifelse}
#' @export
#' @examples \dontrun{when(df$age == 2, df$age + 1)}
setMethod("when", signature(condition = "Column", value = "ANY"),
          function(condition, value) {
              condition <- condition@jc
              value <- if (class(value) == "Column") { value@jc } else { value }
              jc <- callJStatic("org.apache.spark.sql.functions", "when", condition, value)
              column(jc)
          })

#' ifelse
#'
#' Evaluates a list of conditions and returns \code{yes} if the conditions are satisfied.
#' Otherwise \code{no} is returned for unmatched conditions.
#'
#' @family normal_funcs
#' @rdname ifelse
#' @name ifelse
#' @seealso \link{when}
#' @export
#' @examples \dontrun{
#' ifelse(df$a > 1 & df$b > 2, 0, 1)
#' ifelse(df$a > 1, df$a, 1)
#' }
setMethod("ifelse",
          signature(test = "Column", yes = "ANY", no = "ANY"),
          function(test, yes, no) {
              test <- test@jc
              yes <- if (class(yes) == "Column") { yes@jc } else { yes }
              no <- if (class(no) == "Column") { no@jc } else { no }
              jc <- callJMethod(callJStatic("org.apache.spark.sql.functions",
                                            "when",
                                            test, yes),
                                "otherwise", no)
              column(jc)
          })

###################### Window functions######################

#' cume_dist
#'
#' Window function: returns the cumulative distribution of values within a window partition,
#' i.e. the fraction of rows that are below the current row.
#'
#'   N = total number of rows in the partition
#'   cume_dist(x) = number of values before (and including) x / N
#'
#' This is equivalent to the CUME_DIST function in SQL.
#'
#' @rdname cume_dist
#' @name cume_dist
#' @family window_funcs
#' @export
#' @examples \dontrun{cume_dist()}
setMethod("cume_dist",
          signature(x = "missing"),
          function() {
            jc <- callJStatic("org.apache.spark.sql.functions", "cume_dist")
            column(jc)
          })

#' dense_rank
#'
#' Window function: returns the rank of rows within a window partition, without any gaps.
#' The difference between rank and dense_rank is that dense_rank leaves no gaps in ranking
#' sequence when there are ties. That is, if you were ranking a competition using dense_rank
#' and had three people tie for second place, you would say that all three were in second
#' place and that the next person came in third.
#'
#' This is equivalent to the DENSE_RANK function in SQL.
#'
#' @rdname dense_rank
#' @name dense_rank
#' @family window_funcs
#' @export
#' @examples \dontrun{dense_rank()}
setMethod("dense_rank",
          signature(x = "missing"),
          function() {
            jc <- callJStatic("org.apache.spark.sql.functions", "dense_rank")
            column(jc)
          })

#' lag
#'
#' Window function: returns the value that is `offset` rows before the current row, and
#' `defaultValue` if there is less than `offset` rows before the current row. For example,
#' an `offset` of one will return the previous row at any given point in the window partition.
#'
#' This is equivalent to the LAG function in SQL.
#'
#' @rdname lag
#' @name lag
#' @family window_funcs
#' @export
#' @examples \dontrun{lag(df$c)}
setMethod("lag",
          signature(x = "characterOrColumn"),
          function(x, offset, defaultValue = NULL) {
            col <- if (class(x) == "Column") {
              x@jc
            } else {
              x
            }

            jc <- callJStatic("org.apache.spark.sql.functions",
                              "lag", col, as.integer(offset), defaultValue)
            column(jc)
          })

#' lead
#'
#' Window function: returns the value that is `offset` rows after the current row, and
#' `null` if there is less than `offset` rows after the current row. For example,
#' an `offset` of one will return the next row at any given point in the window partition.
#'
#' This is equivalent to the LEAD function in SQL.
#'
#' @rdname lead
#' @name lead
#' @family window_funcs
#' @export
#' @examples \dontrun{lead(df$c)}
setMethod("lead",
          signature(x = "characterOrColumn", offset = "numeric", defaultValue = "ANY"),
          function(x, offset, defaultValue = NULL) {
            col <- if (class(x) == "Column") {
              x@jc
            } else {
              x
            }

            jc <- callJStatic("org.apache.spark.sql.functions",
                              "lead", col, as.integer(offset), defaultValue)
            column(jc)
          })

#' ntile
#'
#' Window function: returns the ntile group id (from 1 to `n` inclusive) in an ordered window
#' partition. Fow example, if `n` is 4, the first quarter of the rows will get value 1, the second
#' quarter will get 2, the third quarter will get 3, and the last quarter will get 4.
#'
#' This is equivalent to the NTILE function in SQL.
#'
#' @rdname ntile
#' @name ntile
#' @family window_funcs
#' @export
#' @examples \dontrun{ntile(1)}
setMethod("ntile",
          signature(x = "numeric"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "ntile", as.integer(x))
            column(jc)
          })

#' percent_rank
#'
#' Window function: returns the relative rank (i.e. percentile) of rows within a window partition.
#'
#' This is computed by:
#'
#'   (rank of row in its partition - 1) / (number of rows in the partition - 1)
#'
#' This is equivalent to the PERCENT_RANK function in SQL.
#'
#' @rdname percent_rank
#' @name percent_rank
#' @family window_funcs
#' @export
#' @examples \dontrun{percent_rank()}
setMethod("percent_rank",
          signature(x = "missing"),
          function() {
            jc <- callJStatic("org.apache.spark.sql.functions", "percent_rank")
            column(jc)
          })

#' rank
#'
#' Window function: returns the rank of rows within a window partition.
#'
#' The difference between rank and denseRank is that denseRank leaves no gaps in ranking
#' sequence when there are ties. That is, if you were ranking a competition using denseRank
#' and had three people tie for second place, you would say that all three were in second
#' place and that the next person came in third.
#'
#' This is equivalent to the RANK function in SQL.
#'
#' @rdname rank
#' @name rank
#' @family window_funcs
#' @export
#' @examples \dontrun{rank()}
setMethod("rank",
          signature(x = "missing"),
          function() {
            jc <- callJStatic("org.apache.spark.sql.functions", "rank")
            column(jc)
          })

# Expose rank() in the R base package
setMethod("rank",
          signature(x = "ANY"),
          function(x, ...) {
            base::rank(x, ...)
          })

#' row_number
#'
#' Window function: returns a sequential number starting at 1 within a window partition.
#'
#' This is equivalent to the ROW_NUMBER function in SQL.
#'
#' @rdname row_number
#' @name row_number
#' @family window_funcs
#' @export
#' @examples \dontrun{row_number()}
setMethod("row_number",
          signature(x = "missing"),
          function() {
            jc <- callJStatic("org.apache.spark.sql.functions", "row_number")
            column(jc)
          })

###################### Collection functions######################

#' array_contains
#'
#' Returns true if the array contain the value.
#'
#' @param x A Column
#' @param value A value to be checked if contained in the column
#' @rdname array_contains
#' @name array_contains
#' @family collection_funcs
#' @export
#' @examples \dontrun{array_contains(df$c, 1)}
setMethod("array_contains",
          signature(x = "Column", value = "ANY"),
          function(x, value) {
            jc <- callJStatic("org.apache.spark.sql.functions", "array_contains", x@jc, value)
            column(jc)
          })

#' explode
#'
#' Creates a new row for each element in the given array or map column.
#'
#' @rdname explode
#' @name explode
#' @family collection_funcs
#' @export
#' @examples \dontrun{explode(df$c)}
setMethod("explode",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "explode", x@jc)
            column(jc)
          })

#' size
#'
#' Returns length of array or map.
#'
#' @rdname size
#' @name size
#' @family collection_funcs
#' @export
#' @examples \dontrun{size(df$c)}
setMethod("size",
          signature(x = "Column"),
          function(x) {
            jc <- callJStatic("org.apache.spark.sql.functions", "size", x@jc)
            column(jc)
          })

#' sort_array
#'
#' Sorts the input array for the given column in ascending order,
#' according to the natural ordering of the array elements.
#'
#' @param x A Column to sort
#' @param asc A logical flag indicating the sorting order.
#'            TRUE, sorting is in ascending order.
#'            FALSE, sorting is in descending order.
#' @rdname sort_array
#' @name sort_array
#' @family collection_funcs
#' @export
#' @examples
#' \dontrun{
#' sort_array(df$c)
#' sort_array(df$c, FALSE)
#' }
setMethod("sort_array",
          signature(x = "Column"),
          function(x, asc = TRUE) {
            jc <- callJStatic("org.apache.spark.sql.functions", "sort_array", x@jc, asc)
            column(jc)
          })











#' This method computes basic statistics on a bigr.vector or a bigr.frame 
#' object. It has several overloaded variants. In its simplest form, the 
#' method accepts a single parameter of class bigr.vector or bigr.frame, and 
#' produces several key statistics such as max, min and count. The method can
#' also be called with a formula that specifies the exact set of aggregate 
#' statistics on one or more columns of a bigr.frame.
#' 
#' The variants of the method that accept one parameter compute a fixed set 
#' of statistics.
#' 
#' The "formula" variant, however, allows for greater flexibility as it 
#' supports the ability to compute grouped aggregate functions for the 
#' specified columns. For a given formula LHS ~ RHS, LHS should contain the 
#' aggregate functions that need to be computed whereas RHS should specify 
#' any grouping columns. If the aggregate functions are to be computed for 
#' the entire dataset (i.e., without grouping), RHS should be set to a dot 
#' (.) symbol. A column alone can be specified in the LHS, and that serves as
#' a shorthand for a collection of aggregate functions that apply to the 
#' column's type.
#' 
#' Supported aggregate functions are: min, max, count, sum, avg, var and sd. 
#' Each function requires a single-parameter that corresponds to a column 
#' name in the bigr.frame. Column names cannot be the same as function names.
#' NA values are implicitly ignored when computing the aggregate statistics 
#' on columns.
#' 
#' The count aggregate has two flavors. count(column) only counts rows where 
#' the column is not NA. In addition, count(.) can be used to count the total
#' number of rows in a bigr.frame, or group. Note that count(column1) and 
#' count(column2) may yield different values because of differences in NA 
#' values across column1 and column2.
#' 
#' @title Compute descriptive statistics
#' @name summary
#' @section Usage:
#'   
#'   \code{summary(object)           # object is a bigr.vector or bigr.frame}
#'   
#'   \code{summary(object, formula)  # object is a bigr.frame}
#'
#' @rdname summary_formula
#' @param object   (bigr.vector or bigr.frame) The data itself
#' @param formula  (formula) Optionally describes a set of columns, aggregate
#'   functions to be calculated on each column, as well any grouping columns.
#'   A formula can only be specified when object is of class "bigr.frame"
#'   
#' @return a data.frame with the computed aggregate functions
#' @examples \dontrun{
#' 
#' #' Summarize a bigr.frame
#' summary(air[,c("DepDelay", "ArrDelay")])
#' 
#' #' Summarize a bigr.vector
#' summary(air$Distance)
#' 
#' # Count total # of rows (flights) in the entire dataset
#' summary(air, count(.) ~ .)
#' 
#' # Count # of flights where Distance != NA
#' summary(air, count(Distance) ~ .)
#' 
#' # Compute basic descriptive statistics (count, min and max) on a
#' # non-numeric column.
#' summary(air, UniqueCarrier ~ .)
#' 
#' # Compute basic descriptive statistics (count, min, max, sum, mean) on a
#' # numeric column.
#' summary(air, air$Distance ~ .)
#' 
#' # Compute mean and standard deviation of distance flown by each airline
#' summary(air, mean(Distance) + sd(Distance) ~ UniqueCarrier)
#' 
#' # Compute a mix of statistics on various columms, grouped
#' # by multiple columns (UniqueCarrier and Year)
#' summary(air, max(Distance) + mean(DepDelay) + ArrDelay 
#'                   ~ UniqueCarrier + Year)
#' }
NULL
bigr.summary <- function(dataset, formula, as.bigr.frame=F) {
 
   ###############################################################################
  # Step 1: Parse left hand side of the formula and build the corresponding JaQL
  # expression for the aggregate functions.
  ###############################################################################
  
  # The JaQL expression containing the aggregated functions. In other words, whatever
  # will be in the "into" clause. Ex: max($[1]), min($[3})
  aggJaqlExp <- ""
  
  # An array containing the column names of the aggregated functions.
  # Ex: max(saleprice), min(askingprice), avg(floors)
  aggColnames <- vector()
  
  # An array containing the column types of the aggregated functions.
  aggColtypes <- vector()
  
  # An array containing the aggregated functions to be applied
  aggFunctions <- vector
  
  # In R, formulas are represented in a tree-like fashion and individual tokens can be
  # extracted using the [[ ]] operators. For example, if formula == max(floors) ~ zipcode,
  # formula[[1]] refers to ~, formula[[2]] is max(floors), and formula[[3]] is zipcode.
  # In turn, formula[[2]][[1]] is max, while formula[[2]][[2]] is floors.
  
  # Therefore, to get the atomic tokens (i.e., aggregate functions and column names), a
  # tree traversal algorithm is invoked for formula[[2]]. Since this algorithm is recursive,
  # a global list of leaves is maintained in bigr.env. Such list must be manually cleaned up
  # before each invocation of the tree traversal algorithm.
  LEAVES <- list()
  leaves <- .bigr.tree.traversal(formula[[2]])
  
  # The data type count
  k <- 1
  
  # Go over the result of the traversal to interpret tokens.
  i <- 1
  while (i <= length(leaves)) {            
    # The column name which an aggregate function will be applied to
    col <- NULL
    
    # The bigr.frame name (only if using $ notation for the columns)
    bfName <- NULL
    
    # The aggregate function name: either max, min, avg, etc.
    fun <- NULL
    
    if (.bigr.isNullOrEmpty(leaves[[i]])) {
      bigr.err(logSource, "Invalid token in the formula: '" %++% leaves[[i]] %++% "'")
    }
    
    # Check if the current leaf is an aggregated function name
    # TODO: Create a list of aggregate functions
    if (leaves[[i]] %in% ALL_AGGREGATE_FUNCTIONS) {
      if (i >= length(leaves)) {
        bigr.err(logSource, "Invalid formula. A column name must be specified for each aggregate function")
      }
      
      # Extract the function name
      fun <- leaves[[i]]
      
      # Once an aggregate function is found, the next symbol(s)
      # will correspond to the column name, which can be one symbol 
      # (e.g., UniqueCarrier), or three symbols (e.g., air$UniqueCarrier)
      if (.bigr.isNullOrEmpty(leaves[[i + 1]])) {
        bigr.err(logSource, "Invalid token in the formula: '" %++% leaves[[i + 1]] %++% "'")
      }
      if (leaves[[i + 1]] == "$") {
        bfName <- leaves[[i + 2]]   
        col <- leaves[[i + 3]]
        i <- i + 4
      } else if (leaves[[i + 1]] == ".") {
        col <- ALL_COLUMNS
        i <- i + 2
      } else {                    
        col <- leaves[[i + 1]]
        i <- i + 2
      }
      
      # If no aggregate function is specified, then a column name was found.
    } else if (.bigr.validSummaryColname(leaves[[i]])) {
      col <- leaves[[i]]
      i <- i + 1            
      
      # If a dollar symbol was found, the next token should be the dataset name and
      # the next one should be the column name
    } else if (leaves[[i]] == "$") {
      if (i >= length(leaves) - 1) {
        bigr.err(logSource, "Invalid formula. A column name must be specified after the $ operator")
      }
      col <- leaves[[i + 2]]
      if (is.null(bfName)) {
        bfName <- leaves[[i + 1]]
      } else if (bfName != leaves[[i + 1]]) {
        bigr.err(logSource, "All columns specified in the formula must belong to the same bigr.frame.")
      } else {
        bfName <- leaves[[i + 1]]
      }
      i <- i + 3
      # + signs will be skipped
    } else if (leaves[[i]] == "+") {
      i <- i + 1
    } else {
      bigr.err(logSource, "Invalid symbol in the formula: '" %++% leaves[[i]] %++% "'")                
    }
    
    # If current symbol/token is not +, some processing needs to be done
    if (!is.null(col)) {            
      
      # If a bigr.frame was specified in the formula
      if (!is.null(bfName)) {
        
        # Check that datasets are consistent
        if (is.null(dataset)) {
          dataset <- get(bfName)
        } else if (.bigr.getJaqlExpression(dataset) != .bigr.getJaqlExpression(get(bfName))) {
          bigr.err(logSource, "All columns specified in the formula must belong to the same bigr.frame.")
        }
      } else if (is.null(dataset)) {
        bigr.err(logSource, "A bigr.frame must be specified in either (1) the left side of the formula, or (2) the 'data' parameter.")
      }
      
      # Get the column id from the given column name
      colid <- NULL
      
      # Handle special cases for count
      if (!.bigr.isNullOrEmpty(fun)) {
        if (fun == "count") {
          if (.bigr.isNullOrEmpty(col)) {
            bigr.err(logSource, "Invalid formula. A column was expected for function count.")
          }
          if (col == ALL_COLUMNS) {                        
            colid <- 0
          } else {
            fun <- "countnonNA"
          }
          # countnonNA will be disabled for the user but not for the backend
        } else if (fun == "countnonNA") {
          bigr.err(logSource, "Invalid function 'countnonNA'. Use count instead.")
        } else {
          if (col == ALL_COLUMNS) {
            bigr.err(logSource, "Operator . is only supported by the count function.")
          }
        }
      }
      if (.bigr.isNullOrEmpty(colid)) {
        colid <- match(col, names(dataset)) - 1
      }
      
      if (.bigr.isNullOrEmpty(colid)) {
        bigr.err(logSource, sprintf("Column '%s' does not belong to the given dataset.", col))
      }
      
      # Get the data type of the current column
      dataType <- coltypes(dataset)[colid + 1]                
      
      # Append a new column to the aggregate JaQL expression if a function was specified
      if (!is.null(fun)) {
        
        invalidOp <- F
        # Check that numeric functions are not being applied to nominal variables
        if (dataType == "character") {
          if (!(fun %in% ALL_NOMINAL_AGGREGATE_FUNCTIONS)) {
            invalidOp <- T
            bigr.warn(logSource, "Cannot apply function '" %++% fun %++% "' to a non-numeric column: " %++% col)
          } else {
            if (fun %in% NUMERIC_TYPE_AGGREGATE_FUNCTIONS) {
              aggColtypes[k] <- "numeric"
            } else {
              aggColtypes[k] <- dataType
            }                            
          }
        } else {
          aggColtypes[k] <- "numeric"
        }
        if (!invalidOp) {
          aggJaqlExp <- aggJaqlExp %++% fun %++% "($[*][" %++% colid %++% "])"
        } else {
          aggColtypes[k] <- "numeric"
          aggJaqlExp <- aggJaqlExp %++% fun %++% "(null)"
        }
        # Display countnonNA as count for the user. Under the covers, we invoke countnonNA
        if (fun == "countnonNA") {
          aggColnames[k] <- "count(" %++% col %++% ")"
        } else {
          aggColnames[k] <- fun %++% "(" %++% col %++% ")"
        }
        aggFunctions[k] <- fun
        k <- k + 1
        
        # Calculate all functions for the current column if no function was specified
      } else {
        
        # Pick the set of aggregate functions according to the data type
        functions <- NULL
        if (dataType == "character" | dataType == "logical") {
          functions <- DEFAULT_NOMINAL_AGGREGATE_FUNCTIONS
        } else {
          functions <- DEFAULT_NUMERIC_AGGREGATE_FUNCTIONS
        }     
        
        aggJaqlExp <- aggJaqlExp %++% paste(functions, "($[*][" %++% colid %++% "])", sep="", collapse=", ")
        aggColFuns <- functions
        for (j in 1 : length(functions)) {
          if (functions[j] %in% NUMERIC_TYPE_AGGREGATE_FUNCTIONS) { 
            aggColtypes[j + k - 1] <- "numeric"                            
          } else {
            aggColtypes[j + k - 1] <- dataType
          }
          if (functions[j] == "countnonNA") {
            aggColnames[j + k - 1] <- "count(" %++% col %++% ")"    
          } else {
            aggColnames[j + k - 1] <- functions[j] %++% "(" %++% col %++% ")"
          }
          
        }
        k <- k + length(functions)
      }
      
      # Append a "," if this is not the last token
      if (i <= length(leaves)) {
        aggJaqlExp <- aggJaqlExp %++% ", "
      }
    }
  }    
  # bigr.info(logSource, "Aggregate JaQL expression: " %++% aggJaqlExp)
  
  ###############################################################################
  # Step 2: Parse right hand size of the formula to extract the grouping columns        
  ###############################################################################
  
  # The JaQL expression for the group by clause (e.g., "$[1], $[2]") 
  groupByJaqlExp <- ""
  
  # The JaQL expression for the grouping columns (e.g., "$[0][1], $[0][2]")
  # This is to include the grouping columns in the resulting data.frame
  groupingColExp <- ""
  
  # The names of the grouping columns
  groupingColnames <- vector()
  
  # The data types of the grouping columns
  groupingColtypes <- vector()
  
  # The complete JaQL query to calculate the summary
  summaryJaqlExp <- NULL
  if (formula[[3]] == ALL_COLUMNS) {
    summaryJaqlExp <- "XXX" %++% " -> " %++% "group into [" %++% aggJaqlExp %++% "]"            
    # bigr.infoShow(logSource, aggColnames)            
  } else {
    # The counter for groupingColnames/types
    k <- 1
    
    # Repeat the same process for formula[[2]] (see above comments)
    LEAVES <- list()
    leaves <- .bigr.tree.traversal(formula[[3]])
    # bigr.infoShow(logSource, leaves)
    i <- 1
    while (i <= length(leaves)) {
      # The column name which an aggregate function will be applied to
      col <- NULL
      
      # The bigr.frame name (optional)
      bfName <- NULL
      
      # If a column name is specified
      if (.bigr.validSummaryColname(leaves[[i]])) {
        col <- leaves[[i]]
        i <- i + 1
        
        # If a dollar symbol was found, the next token should be the dataset name and
        # the next one should be the column name                
      } else if (leaves[[i]] == "$") {
        if (i >= length(leaves) - 1) {
          bigr.err(logSource, "Invalid formula. A column name must be specified after the $ operator")
        }
        col <- leaves[[i + 2]]
        if (is.null(bfName)) {
          bfName <- leaves[[i + 1]]
        } else if (bfName != leaves[[i + 1]]) {
          bigr.err(logSource, "All columns specified in the formula must belong to the same bigr.frame.")
        } else {
          bfName <- leaves[[i + 1]]
        }
        i <- i + 3
      } else if (leaves[[i]] == "+") {
        # Do nothing
        i <- i + 1
      } else {
        stop("Invalid symbol in the formula: '" %++% leaves[[i]] %++% "'")
      }
      
      # If current symbol is not +, there is something to do
      if (!is.null(col)) {
        # If a bigr.frame was specified in the formula
        if (!is.null(bfName)) {
          # Check that datasets are consistent
          if (is.null(dataset)) {
            dataset <- get(bfName)
          } else if (.bigr.getJaqlExpression(dataset) != .bigr.getJaqlExpression(get(bfName))) {
            stop("All columns specified in the formula must belong to the same bigr.frame.")
          }
        } else if (is.null(dataset)) {
          stop("A bigr.frame must be specified in the formula.")
        }
        
        # Get the column id from the given column name
        colid <- match(col, colnames(dataset)) - 1
        if (.bigr.isNullOrEmpty(colid)) {
          stop("2. Invalid column: '" %++% col %++% "'" )
        }
        
        # Append current column to the groupBy clauses
        groupByJaqlExp <- groupByJaqlExp %++% "$[" %++% colid %++% "]"
        #groupingColExp <- groupingColExp %++% "$[0][" %++% colid %++% "]"
        groupingColnames[k] <- col
        groupingColtypes[k] <- coltypes(dataset)[colid + 1]
        k <- k + 1
        if (i <= length(leaves)) {
          groupByJaqlExp <- groupByJaqlExp %++% ", "
          #   groupingColExp <- groupingColExp %++% ", "
        } 
      }
    }
    groupingColExp <- paste("grpcol[" %++% (0:(length(groupingColnames)-1)) %++% "]", collapse=", ")
    
    # bigr.info(logSource, "GroupByJaqlExp: " %++% groupByJaqlExp)
    # bigr.info(logSource, "groupingColnames:")
    # bigr.infoShow(logSource, groupingColnames)
    # bigr.info(logSource, "groupingColtypes:")
    # bigr.infoShow(logSource, groupingColtypes)
    # bigr.info(logSource, "aggColnames:")
    # bigr.infoShow(logSource, aggColnames)
    # bigr.info(logSource, "aggColtypes:")
    # bigr.infoShow(logSource, aggColtypes)
    
    summaryJaqlExp <- "XXX" %++% " -> " %++% "group by grpcol = [" %++% groupByJaqlExp %++%
      "] into [" %++% groupingColExp %++% ", " %++% aggJaqlExp %++% "]"
  }
  class <- NULL
  classType <- NULL
  if (length(groupingColnames > 0)) {        
    class <- "class"
    classType <- "character"
  }
  
  return(summaryJaqlExp)
}

# This function returns the leaves of a formula tree in a preorder fashion
.bigr.tree.traversal <- function(root) {    
  if (!is.null(root)) {
    # If the current node has children
    if (length(root) > 1) {        
      for (i in 1:length(root)) {                
        .bigr.tree.traversal(root[[i]])
      }
      # Only add the current node to the list if it has no children    
    } else {
      LEAVES[[length(LEAVES) + 1]] <- as.character(root)
    }
  }
  LEAVES
}

LEAVES <- list()
ALL_AGGREGATE_FUNCTIONS <- c("max", "min", "avg", "sum")
ALL_NOMINAL_AGGREGATE_FUNCTIONS <- c("min", "max")
ALL_COLUMNS <- "ALL_COLUMNS"
NUMERIC_TYPE_AGGREGATE_FUNCTIONS <- c("avg", "sum")
DEFAULT_NOMINAL_AGGREGATE_FUNCTIONS <- c("min", "max")
DEFAULT_NUMERIC_AGGREGATE_FUNCTIONS <- c("min", "max", "avg", "sum")


.bigr.isNullOrEmpty <- function(x) {
  if (is.null(x)) {
    return(TRUE)  
  } else if (length(x) < 1) {
    return(TRUE)
  }
  if (class(x) == "character" | class(x) == "numeric" | class(x) == "logical" | class(x) == "integer") {
    if (all(is.na(x) | is.null(x) | x == "") ) {
      return(TRUE)
    }
  }
  return(FALSE)
}

.bigr.validSummaryColname <- function(x) { TRUE }

"%++%" <- function(x, y) {
  paste(x, y, sep = "")
}