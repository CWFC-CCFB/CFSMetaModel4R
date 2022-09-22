CBInitialize("localhost", 18000, 50000:50001, 212)
myScript <- new_script("artemis.script.ArtScript")
myScript$getVersion()
myScript$getModelDataFields()
myScript$setEvolutionParameters(as.integer(2016))
myScript$getSpeciesOfType("BroadleavedSpecies")

