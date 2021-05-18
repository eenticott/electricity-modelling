# About Dataset

Data set on residential electricity demand in Ireland, from the CER Smart Metering Project on Electricity Customer Behaviour Trial <http://www.ucd.ie/issda/data/commissionforenergyregulationcer/>. This is not the full data set, but a subset of it.

`Irish` is a list containing `indCons`, `survey`, and `extra`

## Irish$indCons
A matrix where each row is the demand for an individual household in kWh

## Irish$survey
A data.frame containing the following variables:

*`ID` - individual customer ID
*`meanDem` - the mean demand of each customer
*`SOCIALCLASS`, `OWNERSHIP`, ... - see <http://www.ucd.ie/issda/data/commissionforenergyregulationcer/> for details

## Irish$extra
A data.frame containing the following variables:

*`time` - progressive time counter
*`toy` - the time of year from 0 (1st Jan) to 1 (31st Dec)
*`dow` - factor variable indicating the day of the week
*`holy` - binary variable indicating holidays
*`tod` - the time of day, ranging from 0 to 47, where 0 indicates the period from 00:00 to 00:30, 1 the period from 00:30 to 01:00 and so on
*`temp` - the external temperature in degrees Celsius


