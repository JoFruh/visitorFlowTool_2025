#function to sebset agents that recreate on a specific day

  #input:
    #pop = general population to subset from
    #currentDay = the day (week or weekend)

  #out:
    #dayPop = subset population
subsetPopulation <- function(pop, currentDay, AOIList, network, singleAgent = FALSE){

  #subfunctions



  #SIMPLY RETURN POP (NOTHING HAPPENS)

  #For now, there is no subetting of a wider population
  #the agents recreating are a statistical representation of all days from spring to autumn
  #gives an idea of usage, but not of precise, simultaneous usage

#
#   if(singleAgent == TRUE){
#     dayPop <- pop[sample(nrow(pop), 1),]
#
#   }else{
#     #week days
#     if(currentDay >= 1 & currentDay <= 5){
#       #Determine Agents
#       #half old
#       old <- pop[pop$agentTyp == "old", ]
#       dayPop <- old[sample(nrow(old), nrow(old)/2), ]
#       # 1 sport
#       sport <- pop[pop$agentTyp == "sport", ]
#       dayPop <- rbind( dayPop, sport[sample(nrow(sport), 1), ] )
#       #1 nature
#       nature <- pop[pop$agentTyp == "nature", ]
#       dayPop <- rbind( dayPop, nature[sample(nrow(nature), 1), ] )
#
#
#
#     }else{ #weekends
#
#       ##Determine Agents
#       #half of all agents
#       dayPop <- pop[sample(nrow(pop), nrow(pop)/2), ]
#
#
#     }
#
#   }
  ##TODO: remove simple row determination
  # if(!is.null(agentCount)){
  #   return(pop)
  # }
  return(pop)
}
