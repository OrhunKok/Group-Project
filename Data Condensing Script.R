library(dplyr)


Data = read.csv("database.csv")
View(Data)

unique(Data$Species.Name)



length(unique(Data$Species.Name))
table(Data$Species.Name)

Data -nco

Data = select(Data,-Operator,-Operator.ID,-Aircraft,-Aircraft.Type,-Aircraft.Make,-Aircraft.Model,
                -Aircraft.Mass,-Engine.Make,-Engines,-Engine.Model,-Engine.Type,-Engine1.Position,-Engine2.Position,
                -Engine3.Position,-Engine4.Position,-Fatalities,-Injuries,-Aircraft.Damage,-Radome.Strike,-Radome.Damage,
                -Windshield.Strike,-Windshield.Damage,-Nose.Strike,-Nose.Damage,-Engine1.Strike,-Engine1.Damage,
                -Engine2.Strike,-Engine2.Damage,-Engine3.Strike,-Engine3.Damage,-Engine3.Strike,-Engine3.Damage,
                -Engine4.Strike,-Engine4.Damage,-Engine.Ingested,-Propeller.Strike,-Propeller.Damage,-Wing.or.Rotor.Strike,
                -Wing.or.Rotor.Damage,-Fuselage.Strike,-Fuselage.Damage,-Landing.Gear.Strike,-Landing.Gear.Damage,
                -Tail.Strike,-Tail.Damage,-Lights.Strike,-Lights.Damage,-Other.Strike,-Other.Damage)

write.csv(Data,"C:\\Users\\koko\\Downloads\\NewDataset.csv")

