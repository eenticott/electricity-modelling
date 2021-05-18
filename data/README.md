# About Dataset

Data set on residential electricity demand in Ireland, from the CER Smart Metering Project on Electricity Customer Behaviour Trial <http://www.ucd.ie/issda/data/commissionforenergyregulationcer/>. 

This is a subset of the full data set.

`Irish` is a list containing `indCons`, `survey`, and `extra`

### indCons
A matrix where each row is the demand for an individual household in kWh.

### survey
A data.frame containing the following variables:

* `ID` - individual customer ID
* `meanDem` - the mean demand of each customer
* `SOCIALCLASS` - code for occupation of chief income earner (5 levels)
* `OWNERSHIP` - rent or own home (2 levels)
* `BUILT.YEAR` - year home was built 
* `HEAT.HOME` - home heated using electricity or other
* `HEAT.WATER` - water heated using electricity or other
* `WINDOWS.doubleglazed` - amount of windows double glazed e.g. All or Half (5 levels)
* `HOME.APPLIANCE..White.goods.` - how many appliances, such as washing machine, dishwasher (0-5) 
* `Code` - all are 2673?
* `ResTariffallocation` - participants were allocated to different tariff groups (6 levels)
* `ResStimulusallocation` - participants were allocated to different stimulus groups (6 levels)

See <http://www.ucd.ie/issda/data/commissionforenergyregulationcer/> for details.

### extra
A data.frame containing the following variables:

* `time` - progressive time counter
* `toy` - the time of year from 0 (1st Jan) to 1 (31st Dec)
* `dow` - factor variable indicating the day of the week
* `holy` - binary variable indicating holidays
* `tod` - the time of day, ranging from 0 to 47, where 0 indicates the period from 00:00 to 00:30, 1 the period from 00:30 to 01:00 and so on
* `temp` - the external temperature in degrees Celsius


