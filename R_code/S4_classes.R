# S4 Classes
# 
# Author: OGOREKB
###############################################################################


# Time series class with three attributes, also known as "slots" #
setClass("TimeSeries", 
		representation(
				data="numeric",
				start = "POSIXct", # the classes "POSIXlt" and "POSIXct" representing calendar dates and times (to the nearest second). 
				end = "POSIXct"
					  )
		)

# The new() function is a generic constructor for S4 objects #
my.TimeSeries <- new("TimeSeries",
		data = c(1,2,3,4,5,6),
		start = as.POSIXct("07/01/2009 0:00:00", tz = "GMT", format = "%m/%d/%Y %H:%M:%S"),
		end = as.POSIXct("07/01/2009 0:05:00", tz = "GMT", format = "%m/%d/%Y %H:%M:%S")
		)

my.TimeSeries					
					
# If the slots hold the data, then setValidity() sets the constraints#
setValidity("TimeSeries", 
		function(object) {
			object@start <= object@end &&
			length(object@start) == 1 &&
			length(object@end) == 1
	}
)
	
validObject(my.TimeSeries)

# Setting up a method that can use the class TimeSeries, since there is a "data" slot
series <- function(object) {object@data}
series
series(my.TimeSeries)

#Now we redefine series as a generic function with the old definition as the default method
setGeneric("series")
series
showMethods("series")

# summary is a standard generic summary. Here I define a summary method for my new class #
# Think: I'm overloading the summary function, just like _add_, or _print_ in python
setMethod("summary",
		signature = "TimeSeries",
		definition = function(object){
			print(paste(object@start, " to ", object@end, sep = "", collapse=","))
		}
	)
	
summary(my.TimeSeries)
### For a complete list of generic functions: ###
help(S4groupGeneric)

# Inheritance: extending classes, in the simple case of adding new slots##
setClass("WeightHistory",
		representation (height = "numeric", name = "character"),
		
		contains = "TimeSeries"
)

my.WeightHistory = new("WeightHistory",
		data = c(1,2,3,4,5,6),
		start = as.POSIXct("07/01/2009 0:00:00", tz = "GMT", format = "%m/%d/%Y %H:%M:%S"),
		end = as.POSIXct("07/01/2009 0:05:00", tz = "GMT", format = "%m/%d/%Y %H:%M:%S"),
		height = 72,
		name = "Ben Ogorek")

# Creating a virtual Class, or superclass, where we can define common functions. #
# While this seems interface-like, it is not an interface. Abstract classes can have structure, code.
#A class may be defined as the union of other classes; that is, as a virtual class defined as a superclass of several other classes.#
setClassUnion("NamedThing",
		c("Person", "Cat")
	)
#We could create a function that applies to Namedthing, and use it on either Person or Cat objects#

# Getting and Setting #
my.WeightHistory@name
#or#
slot(my.WeightHistory, name = 'name')

### creating CoersionMethods ####

setAs("numeric", "TimeSeries", function(from){
			new("TimeSeries",
					data = from*c(1,2,3,4,5,6),
					start = as.POSIXct("07/01/2009 0:00:00", tz = "GMT", format = "%m/%d/%Y %H:%M:%S"),
					end = as.POSIXct("07/01/2009 0:05:00", tz = "GMT", format = "%m/%d/%Y %H:%M:%S")
			)
		}
)

#convert the numeric object 6 to a TimeSeries object
as(6,"TimeSeries")


## S3 classes ##
## Using in S4 class building: setOldClass
## simple generic functions that work because of a naming convention : UseMethod() and NextMethod()
