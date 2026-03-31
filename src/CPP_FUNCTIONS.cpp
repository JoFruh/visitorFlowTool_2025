#include <Rcpp.h>
#include <iostream>
#include <limits.h>
#include <queue>
#include <algorithm>
#include <fstream>
#include <unordered_map>
#include <unordered_set>

using namespace Rcpp;



/*
 * filterRouteChoices:
 * takes neighbours, currentVs, lastVs, retracedVs and non-AOI nodes (for all agents),
 * determines which neighbours are viable routes (not in current, prior or retraced Vs, and not outside AOI)
 */

// [[Rcpp::export]]
List filterRouteChoices_cpp(List neighbours, NumericVector currentVs, NumericVector lastVs, List retracedVs, NumericVector V_outside_aoi){//,  List priorVs,

  //get length of neighbours

  int n_nghb = neighbours.size();
  // Rprintf("n_nghb: %i",n_nghb );
  //prepare List of same length as neighbours
  List finalOutput(n_nghb);

  // Build unordered_set of outside-AOI nodes ONCE for O(1) membership testing
  std::unordered_set<int> outside_aoi_set(V_outside_aoi.begin(), V_outside_aoi.end());

  // loop through List of neighbours
  for(int agentNo = 0; agentNo < n_nghb; agentNo++){

    //get List subsets
    NumericVector nghb = neighbours[agentNo];

    int crrnt = currentVs[agentNo];
    int lst = lastVs[agentNo];

    // Build retraced set for O(1) membership testing per agent
    std::unordered_set<int> retraced_set;
    if(retracedVs[agentNo] != R_NilValue){
      NumericVector rtrcd = retracedVs[agentNo];
      retraced_set.insert(rtrcd.begin(), rtrcd.end());
    }

    //determine how many neighbours
    int n = nghb.size();

    //prepare output container
    std::vector<int> output;

    //keep track of how many routes are kept
    int keptVariableTracking = 0;
    //cycle through neighbours, filtering out current, last, retraced, or outside-AOI nodes
    for (int j=0; j < n; ++j) {
      int node = (int)nghb[j];

      //when the node passes all filters, include it in the final output
      if (node != crrnt && node != lst &&
          !retraced_set.count(node) &&
          !outside_aoi_set.count(node)){

        //count kept routes
        keptVariableTracking++;

        //append to results
        output.push_back(node);
      }
    }

    //when no routes are kept
    if(keptVariableTracking == 0){

      //no routes kept, return empty list
      finalOutput[agentNo] = R_NilValue;

    }else{

      //

      //add output vector to finalOutput
      finalOutput[agentNo] = output;

    }



  }


    return finalOutput;

}



/*
 * chooseBestRoute:
 * takes a selection of possible routes (for every agent),
 * determines which route is the most attractive, and shortest in ties (for each agent)
 */

// [[Rcpp::export]]
List chooseBestRoutes_cpp(List viableRoutes, DataFrame DULN_df, List priorVs, StringVector agentTyps, DataFrame V_coords_df, NumericVector currentVs){

  //length of list of agents
  int nbOfAgents = viableRoutes.size();

  List outputList = List(nbOfAgents, NA_REAL);

  NumericVector DULN_df_nodeID = DULN_df["nodeID"];
  NumericVector V_coords_df_X = V_coords_df["X"];
  NumericVector V_coords_df_Y = V_coords_df["Y"];

  // Build nodeID -> row-index map ONCE for O(1) lookups in all inner loops
  std::unordered_map<int, int> nodeID_to_row;
  nodeID_to_row.reserve(DULN_df_nodeID.size());
  for(int i = 0; i < DULN_df_nodeID.size(); i++){
    nodeID_to_row[(int)DULN_df_nodeID[i]] = i;
  }


  //prepare variables for angle calculations
  float pv_x = 1;
  float pv_y = 1;
  float nv_x = 1;
  float nv_y = 1;
  float cv_x = 1;
  float cv_y = 1;

  //cycle through agents
  for(int agentNo = 0; agentNo < nbOfAgents; agentNo++){

    String DULN_choice = " ";
    //determine which column of attractivity values to use (which agent)
    if(agentTyps[agentNo] == "walkNat"){
      DULN_choice = "DULN_WALK_";
    }else if(agentTyps[agentNo] == "walkSoc"){
      DULN_choice = "DULN_WALK1";
    }else if(agentTyps[agentNo] == "dogNat"){
      DULN_choice = "DULN_DOG_N";
    }else if(agentTyps[agentNo] == "dogProx"){
      DULN_choice = "DULN_DOG_P";
    }else if(agentTyps[agentNo] == "ebikeNat"){
      DULN_choice = "DULN_EBIKE";
    }else if(agentTyps[agentNo] == "bikeSport"){
      DULN_choice = "DULN_BIKER";
    }else if(agentTyps[agentNo] == "jogger"){
      DULN_choice = "DULN_JOGGE";
    }else{
      Rprintf("ERROR: no match for agentTyp");
      // String test = agentTyps[agentNo];
      // Rprintf(test);

      // Rcpp::Rcout << test << std::endl;

    }



    //determine current values
    NumericVector agentRoutes = viableRoutes[agentNo];

    //prepare var for routes in PriorVs
    NumericVector agentPriorVs;
    //determine routes that are PriorVs
    LogicalVector routesInPrr;

    // Rcpp::Rcout << "routesInPrr SET" << std::endl;

    //Giving priority to non-priorV routes
    //Determine if some viable Routes are NOT in priorVs
    //only if there are priorVs
    if(priorVs[agentNo] != R_NilValue){

      //determine current vertex (cv) to determine angle later — O(1) map lookup
      {
        auto cv_it = nodeID_to_row.find((int)currentVs[agentNo]);
        if(cv_it != nodeID_to_row.end()){
          cv_x = V_coords_df_X[cv_it->second];
          cv_y = V_coords_df_Y[cv_it->second];
        }
      }

      //prepare var for routes in PriorVs
      agentPriorVs = priorVs[agentNo];
      //determine routes that are PriorVs
      routesInPrr = in(agentRoutes, agentPriorVs);

      //is there at least one route not within PriorVs
      if( sum(routesInPrr) < agentRoutes.size() ){
        //instead of removing the path in Prr, set them a probability of 0 (later in the code)
      }

      //determine pv vector based on priorV id — O(1) map lookup
      {
        auto pv_it = nodeID_to_row.find((int)agentPriorVs[agentPriorVs.size()-1]);
        if(pv_it != nodeID_to_row.end()){
          pv_x = V_coords_df_X[pv_it->second];
          pv_y = V_coords_df_Y[pv_it->second];
        } else {
          Rcpp::Rcout << "ERROR: COULD NOT FIND NODE ID FOR PRIORV" << std::endl;
        }
      }
    }else{
      //if there is no priorV, do not determine angle
      pv_x = 0;
      pv_y = 0;
    }



    int routesLength = agentRoutes.size();

    // Extract DULN column ONCE per agent (outside the route loop)
    NumericVector DULN_col = DULN_df[DULN_choice];

    //prepare variables for sum of all DULN_values
    double DULNexpSum = 0;
    double alpha = 0.5;
    std::vector<double> v_DULN_values(routesLength);

    for(int routeNo = 0; routeNo < routesLength; routeNo++ ){

      //determine each node's DULN value via O(1) map lookup
      double DULN_value = 0;
      auto it = nodeID_to_row.find((int)agentRoutes[routeNo]);
      if(it != nodeID_to_row.end()){
        int rowNo = it->second;
        DULN_value = DULN_col[rowNo];
        if( R_IsNA(DULN_value) ){
          DULN_value = 0;
        }
        nv_x = V_coords_df_X[rowNo];
        nv_y = V_coords_df_Y[rowNo];
      }


      // //DETERMINE ANGLE ///
      // //get angle based on prior (pv), current V (cv) and potential next vertex (nv)
      // rad = atan2(cv_y - pv_y, cv_x - pv_x) - atan2(cv_y - nv_y, cv_x - nv_x)
      // degree = rad * 180/pi

      // only do this if agent has a direction (there are priorVs)
      if(pv_x != 0 & pv_y != 0){

        //determine cv



        //calculate angle
        float angle = abs(atan2(cv_y - pv_y, cv_x - pv_x) - atan2(cv_y - nv_y, cv_x - nv_x) * (180/3.14159265359));

        //translate angle to 0-180
        if(angle > 180){
          angle = 180 - (angle - 180);
          }

        // Rcpp::Rcout << "angle: " << angle<< std::endl;

        //add or remove attr depending on orientaton of path

        if(angle <= 45){
          DULN_value = DULN_value + 1;
        }else if(angle > 45 & angle <= 90){
          DULN_value = DULN_value + 0;
        }else if(angle > 90 & angle <= 180){
          DULN_value = DULN_value -2;
        }
      }

      //Deterministic way of choosing best route
      // if(DULN_value > highestDULN){
      //   highestDULN = DULN_value;
      //   bestRouteNo = routeNo;
      // }

      // stochastic way:
      // sum all DULN_value, then do exp(x)/sum(exp(each x))
      // output that to use as probability in sampling
      DULNexpSum = DULNexpSum + exp(DULN_value/alpha);
      v_DULN_values[routeNo] = DULN_value;

      // Rcpp::Rcout << "DULN_col size :" << v_DULN_values.size() << std::endl;
      // Rcpp::Rcout << "rowNo :" << routeNo << std::endl;
    }


    // DETERMINISTIC method
    //int bestRoute = agentRoutes[bestRouteNo];
    //outputList[agentNo] = bestRoute;


    //STOCHASTIC method
    NumericVector bestRouteProbs(v_DULN_values.size(), NA_REAL);



    for(int valueNo = 0; valueNo < bestRouteProbs.size(); valueNo++){

      bestRouteProbs[valueNo] = exp(v_DULN_values[valueNo]/alpha)/DULNexpSum;
    }

    //TODO: sample within cpp for speed.
    //but Rcpp::sample not working
    //sample best routeNo
    // std::vector<int> bestRouteSeq = seq(1, agentRoutes.size());
    // int n = 1;
    // bestRouteNo = sample(bestRouteSeq, n, false, bestRouteProbs);
    //
    // int bestRoute = agentRoutes[bestRouteNo];

    //after determining probabilities, set prob of PriorV routes to a very low value (0.00001)
    //Rcpp::Rcout << "routesInPrr: "<< routesInPrr << std::endl;

    if(routesInPrr.size() > 1){
      bestRouteProbs[routesInPrr] = 0.00001;
    }



    outputList[agentNo] = bestRouteProbs;
  }

  return outputList;
  // use outputList to sample from viableRoutes with bestRouteProbs as sampling probabilities

  /*
   chooseBestRoutes <- function(viable){

    /transform edge ids into vs
   viableVS <- V(network)[V(network)$nodeID %in% viable]

    /TODO: viableES <-> viable (whether cpp function is used or not)
   bestChoice <- viableVS[viableVS$DULN == max(viableVS$DULN)]
   if(length(bestChoice)>1){
   bestChoice <- sample(bestChoice, 1)
   }
   bestChoice
   }
   routeChoices[areViableChoices] <- mapply( chooseBestRoutes,
   routeChoices[areViableChoices])
   */

}


/*
 * getEdges:
 * takes every agents' NextV and CurrentVs,
 * returns a vector of edgeIDs
 */

// [[Rcpp::export]]
List getEdges_cpp(IntegerVector currentVs, IntegerVector nextVs, DataFrame edgeTable, bool usingPathToGoal, List pathToGoal, List oldPriorEs){

  //TO vector
  IntegerVector To_v = edgeTable["to"];

  //FROM vector
  IntegerVector From_v = edgeTable["from"];

  //edgeID vector
  IntegerVector edgeID_v = edgeTable["edgeID"];

  // Build edge lookup map ONCE: encode(from,to) -> edgeID for O(1) lookup
  // Uses bit-packing: upper 32 bits = one node, lower 32 bits = other node
  std::unordered_map<int64_t, int> edgeMap;
  edgeMap.reserve(To_v.size() * 2);
  for(int i = 0; i < To_v.size(); i++){
    edgeMap[((int64_t)To_v[i]   << 32) | (uint32_t)From_v[i]] = edgeID_v[i];
    edgeMap[((int64_t)From_v[i] << 32) | (uint32_t)To_v[i]]   = edgeID_v[i];
  }
  // Rcpp::Rcout << "edgeID_v: " << edgeID_v << std::endl;

  //prepare output container
  IntegerVector allEdgeIDs = IntegerVector(nextVs.size());

  int nxt;
  int crrnt;
  int edgeID= 0;
  //prepare final container for priorEs
  std::vector<std::vector<int>> allPriorEs(currentVs.size());
// cycle through all agents
  for(int agentNo = 0; agentNo < currentVs.size(); agentNo++){
    // Rcpp::Rcout << "agentno: " << agentNo << std::endl;


    if(usingPathToGoal == false){

    //note this agent's currentV
      crrnt = currentVs[agentNo];

    }else{

      //if there is a pathToGoal (shortest and nicestShortest behaviours)
      //use before-last node of the pathToGoal to determine currentV
      //currentE will represent last path before arriving at goalV (last node of pathToGoal)

      IntegerVector path = pathToGoal[agentNo];

      //get before-last node: -2 because -1 gets LAST node (in C++ first component is 0 and not 1)
      //However, if there's only 1 node in pathToGoal, then startV (currentV) is the before-last node
      if(path.size() > 1){
        crrnt = path[path.size()-2];

        std::vector<int> priorEs;
        //CONVERT PriorVs TO priorE (edge history)
        //cycle through priorVs (ignoring last two vertices which are currentE)
        int n = path.size()-2;
        // Rcpp::Rcout << "n: " << n << std::endl;

        for(int prrV_aNo = 0; prrV_aNo < n; prrV_aNo++){

          //get priorV pair and look up edge in O(1)
          int prrV_a = path[prrV_aNo];
          int prrV_b = path[prrV_aNo + 1];

          int64_t prKey = ((int64_t)prrV_a << 32) | (uint32_t)prrV_b;
          auto prIt = edgeMap.find(prKey);
          if(prIt != edgeMap.end()){
            priorEs.push_back(prIt->second);
          }
        }


        //add priorEs to container for all agents
        std::vector<int> newAgentPriorE;

        //if agentPriorE is not a null value
        if(oldPriorEs[agentNo] != R_NilValue){
          //get prior Es
          std::vector<int> agentPriorE = oldPriorEs[agentNo];
          //append vector to existing agentPriorE
          //prepare new container of larger size
          newAgentPriorE = std::vector<int> (agentPriorE.size() + priorEs.size());

          //iterate and copy elements from agentPriorE and priorEs to newAgentPriorE
          for(int i = 0; i < agentPriorE.size(); i++){
            newAgentPriorE[i] = agentPriorE[i];
          }
          for(int j = 0; j < priorEs.size(); j++){
            newAgentPriorE[agentPriorE.size() + j] = priorEs[j];
          }

        }else{
          //otherwise
          //create new list with agentPriorE
          newAgentPriorE = std::vector<int>(1);
          newAgentPriorE = priorEs;
        }

        //add to final container
        allPriorEs[agentNo] = newAgentPriorE;
        // Rcpp::Rcout << "newAgentPriorE: " << wrap(newAgentPriorE) << std::endl;

      }else{
        crrnt = currentVs[agentNo];
      }



      // Rcpp::Rcout << "crrnt: " << crrnt << std::endl;

    }

    nxt = nextVs[agentNo];

    // O(1) edge lookup via hash map
    {
      int64_t edgeKey = ((int64_t)crrnt << 32) | (uint32_t)nxt;
      auto edgeIt = edgeMap.find(edgeKey);
      if(edgeIt != edgeMap.end()){
        edgeID = edgeIt->second;
      } else {
        edgeID = 0;
      }
    }

    //catch error
    if(edgeID == 0){
      Rcpp::Rcout << "agentNo: " << agentNo << std::endl;
      Rcpp::Rcout << "edgeID == 0 >> nextV: " << nxt << std::endl;
      Rcpp::Rcout << "edgeID == 0 >> currentV: " << crrnt << std::endl;
    }

    allEdgeIDs[agentNo] =  edgeID;

  }

  List allEdgeIDs_list = List::create(wrap(allEdgeIDs));
  List allPriorEs_list = List::create(wrap(allPriorEs));

  List output = List::create(Named("currentE") = allEdgeIDs_list, Named("priorE") = allPriorEs_list);

  return  output;
}





/*
 * getVisitedEdges:
 * uses nextVs and lastPriorVs to determine the visited edge for every agent that just moved
 */
//DEPRECATED - TO REMOVE///

// [[Rcpp::export]]
IntegerVector getVisitedEdges_cpp(IntegerVector nextVs,
                    IntegerVector lastPriorVs,
                    DataFrame edgeTable){


    //prepare vectors from dataframe
    IntegerVector To_v = edgeTable["to"];
    IntegerVector From_v = edgeTable["from"];
    IntegerVector edgeID_v = edgeTable["edgeID"];

    IntegerVector output = IntegerVector(nextVs.size());

    // cycle through all agents
    for(int nxtNo = 0; nxtNo < nextVs.size(); nxtNo++){

      //note this agent's nextV
      int nxt = nextVs[nxtNo];
      // Rcpp::Rcout << "crrnt: " << crrnt << std::endl;

      int prr = lastPriorVs[nxtNo];
      // Rcpp::Rcout << "nxt: " << nxt << std::endl;

      int edgeID= 0;

      //cycle through all edgeTable rows
      for(int rowNo = 0; rowNo < To_v.size(); rowNo++){

        //for every hit in the To column, check for a hit for NextVs in the other From column
        if( (nxt == To_v[rowNo]) & (prr == From_v[rowNo]) ){
          //note edgeID
          edgeID = edgeID_v[rowNo];
          // Rcpp::Rcout << "edgeID: " << edgeID << std::endl;

          break;

        }else if( (prr == To_v[rowNo]) & (nxt == From_v[rowNo] ) ){//other check the inverse
          //note edgeID
          edgeID = edgeID_v[rowNo];
          // Rcpp::Rcout << "edgeID: " << edgeID << std::endl;

          break;

        }

      }

      output[nxtNo] = edgeID;

  }
  return output;
}


/*
# ## TODO : INCREASE SPEED (Priority 2)####
# #get the visited edges based on nextV (nextV will actually be currentV next few lines)
# visitedEdges <- mapply(function(nxt, prr) edgeTable$edgeID[edgeTable$from %in% c(nxt, prr) & edgeTable$to %in% c(nxt, prr )],
#                                  dayPop$nextV[agentsReachedNextV],
#                                  lastPriorVs(dayPop$priorV[agentsReachedNextV]))

#CPP FUNCTION: getVisitedEdges_cpp
visitedEdges <- getVisitedEdges_cpp(dayPop$nextV[agentsReachedNextV],
                                    lastPriorVs(dayPop$priorV[agentsReachedNextV]))

*/


// List retracing_cpp(){
//
// }


/*
 * updateNicestHistory:
 * takes currentVs and nicestPriorVs of agents with "nicest" behaviour, and returns an updated history of nicestPriorVs
 */

// [[Rcpp::export]]
List updateNicestHistory_cpp(List nicestPriorVs, std::vector<int> nicestCurrentVs){
  //prepare container
  List output = List(nicestCurrentVs.size());

  int n = nicestCurrentVs.size();

  //prepare an empty update vector to append to. Replace it with nicestPriorVs if they exist.
  for(int agentNo = 0; agentNo < n; agentNo++){

    std::vector<int> update = std::vector<int>(0);

    //cast NumericVector to vector<int> (more efficient)
    if(nicestPriorVs[agentNo] != R_NilValue){
    update = as<std::vector<int>>(nicestPriorVs[agentNo]);
    }
    //append int currentV id
    update.push_back(nicestCurrentVs[agentNo]);

    output[agentNo] = update;
  }

  return output;

}

/*
 * retraceSteps:
 * insert agent dataframe and neighbourhoodVs,
 * return modified agent dataframe
 */

// [[Rcpp::export]]
List retraceSteps_cpp(std::vector<int> currentVs, std::vector<int> nextVs, List priorVs, List retracedVs  ){

  // Rcpp::Rcout << "TEST0" <<  std::endl;;


  //get relevant vectors of df (get them as input instead)
  // List priorVs = agent_df["priorV"];
  // std::vector<int> currentVs = agent_df["currentV"];
  // std::vector<int> nextVs = agent_df["nextV"];
  // List retracedVs = agent_df["retracedV"];
  // StringVector behaviour = agent_df["behaviour"];

  //prepare result container
  // DataFrame output = DataFrame(agent_df);
  // Rcpp::Rcout << "routes:" <<  routes <<  std::endl;
  // Rcpp::Rcout << "routes[1]:" << routes[1] <<  std::endl;
  // Rcpp::Rcout << "routes[1] != R_NilValue:" << routes[1] != R_NilValue <<  std::endl;

  // Rcpp::Rcout << "currentVs.size(): "<< currentVs.size() <<  std::endl;



  int n = currentVs.size();

  // Rcpp::Rcout << "TEST0b" <<  std::endl;;

  List priorResults = List(n);

  List retracedResults = List(n);

  std::vector<int> nextVResults = std::vector<int>(n);

  //loops through all agents
  for(int agentNo = 0; agentNo < n; agentNo++){

    // Rcpp::Rcout << "currentVs[agentNo]:" << " " << currentVs[agentNo] <<  std::endl;

    //stepback further
    //last priorV -> nextV and remove last priorV
    // Rcpp::Rcout << priorVs[agentNo]) <<  std::endl;;

    std::vector<int> agentPriorV = as<std::vector<int>>( priorVs[agentNo] );
    nextVResults[agentNo] = agentPriorV.back();

    // Rcpp::Rcout << "TEST1" <<  std::endl;;

    //only remove element if there is at least 2
    if(agentPriorV.size() > 1){
      agentPriorV.pop_back();
      priorResults[agentNo] = agentPriorV;
    }else{
      // create a list(NULL) instead.
      priorResults[agentNo] = R_NilValue;
    }

    // Rcpp::Rcout << "TEST2" <<  std::endl;;

    std::vector<int> agentRetraced;
    //currentV -> retraced (make currentV NA?)
    if(retracedVs[agentNo] == R_NilValue){
      agentRetraced = std::vector<int>{currentVs[agentNo]};
    }else{
      agentRetraced = as<std::vector<int>>(retracedVs[agentNo]);
      agentRetraced.push_back(currentVs[agentNo]);
    }

    // Rcpp::Rcout << "TEST3" <<  std::endl;;

    retracedResults[agentNo] = agentRetraced;

    currentVs[agentNo] = NA_REAL;

    // Rcpp::Rcout << "TEST4" <<  std::endl;;

  }

  // Rcpp::Rcout << "Finished" <<  std::endl;


  //return results as a named list
  return Rcpp::List::create(
    Named("priorV") = priorResults,
    Named("retracedV") = retracedResults,
    Named("nextV") = wrap(nextVResults)
    );

  //
  //
  //

}

//
// retraceSteps <- function(agentID){
// #               agent <- dayPop[dayPop$id == agentID, ]
// #
// #               #check if there are other unexplored possibilities (only remove current and retraced vertices)
// #               routeChoices <- neighborhood(network, 1, agent$currentV)[[1]]
// #
// #               #not current position
// #               routeChoices <- routeChoices[routeChoices$nodeID != agent$currentV]
// #               #not retraced
// #               routeChoices <- routeChoices[!routeChoices$nodeID %in% agent$retracedV[[1]]]
// #               #not visited prior
// #               routeChoices <- routeChoices[!routeChoices$nodeID %in% agent$priorV[[1]]]
// #
// #               if(length(routeChoices) > 0){
// #                 #if there are choices
// #                 #choose most attractive route,
// #                 bestChoice <- routeChoices$nodeID[routeChoices$DULN == max(routeChoices$DULN)]
// #                 #if tied randomise choice
// #                 if(length(bestChoice) > 1){bestChoice <- sample(bestChoice, 1)}
// #                 #return to "nicest" behaviour
// #                 agent$behaviour <- "nicest"
// #
// #
// #               }else{
// #                 debugRetracing[agentID] <<- debugRetracing[agentID] + 1
// #
// #                 if(debugRetracing[agentID] > 100){browser()}
// #                 #if there still no choices
// #                 #back up further
// #                 if(is.null(agent$priorV[[1]])){browser()}
// #                 #make the earlier prior the nextV,
// #                 bestChoice <- agent$priorV[[1]][[length(agent$priorV[[1]])]]
// #
// #                 #remove the last priorV (put NULL if no priorVs left)
// #                 if( ( length(agent$priorV[[1]])-1 ) < 1){
// #                   agent$priorV <- list(NULL)
// #                 }else{
// #                   agent$priorV <- list(agent$priorV[[1]][1:(length(agent$priorV[[1]])-1)])
// #                 }
// #                 #and place currentV in retracedV
// #                 agent$retracedV <- list(c(agent$retracedV[[1]], agent$currentV))
// #
// #
// #               }
// #
// #               return(agent)
// #
// #             }




/*
 * historyToPassage:
 * use agentTable, edgeTable and vertexTable,
 * to translate vertex/edge history (ex: priorV, priorE, retracedV) into passage vectors
 * that can then be inserted in vertex and edgeTable
 */

// [[Rcpp::export]]
List historyToPassage_cpp(DataFrame agentTable, DataFrame edgeTable, DataFrame vertexTable){

  //prepare history
  //vertex
  List vertexHistoryPrior = agentTable["priorV"];
  List vertexHistoryRetracing = agentTable["retracedV"];
  //edge
  List edgeHistory = agentTable["priorE"];
  List edgeHistoryAOI = agentTable["priorEAOI"];

  List agentTypes = agentTable["agentTyp"];

  // Rcpp::Rcout<<"step1"<<std::endl;

  //prepare vertex vector to contain passage (vector positions reflect vertex IDs (id 13 = 13th position etc))
  //add +1 to length so that last element x can be at position x, rather than x-1 (as usual in c++)
  IntegerVector vID = vertexTable["nodeID"];
  int maxvID = Rcpp::max(vID);
  // Rcpp::Rcout<<"Max nodeID: "<<maxvID<<std::endl;

  IntegerVector vertexPassage = IntegerVector(maxvID + 1);

  IntegerVector eID = edgeTable["edgeID"];
  int maxeID = Rcpp::max(eID);
  // Rcpp::Rcout<<"Max edgeID: "<<maxeID<<std::endl;

  // Rcpp::Rcout<<"step3"<<std::endl;

  IntegerVector edgePassage = IntegerVector(maxeID + 1);
  IntegerVector edgePassageAOI = IntegerVector(maxeID + 1);

  IntegerVector edgePassageWalk = IntegerVector(maxeID + 1);
  IntegerVector edgePassageWalkAOI = IntegerVector(maxeID + 1);

  IntegerVector edgePassageDog = IntegerVector(maxeID + 1);
  IntegerVector edgePassageDogAOI = IntegerVector(maxeID + 1);

  IntegerVector edgePassageBike = IntegerVector(maxeID + 1);
  IntegerVector edgePassageBikeAOI = IntegerVector(maxeID + 1);

  IntegerVector edgePassageJog = IntegerVector(maxeID + 1);
  IntegerVector edgePassageJogAOI = IntegerVector(maxeID + 1);


  // Rcpp::Rcout<<"step4"<<std::endl;

  //cycle through all agents
  for(int agentNo = 0; agentNo < agentTable.nrow(); agentNo++){
    // Rcpp::Rcout<<"agentNo: "<<agentNo<<std::endl;

    // Rcpp::Rcout<<"step5"<<std::endl;

    //determine agent's history
    //vertices

    IntegerVector currentHistoryPrior;

    if(vertexHistoryPrior[agentNo] != R_NilValue){
      currentHistoryPrior = vertexHistoryPrior[agentNo];
    }
    // Rcpp::Rcout<<"agentNo: "<<agentNo<<std::endl;
    // Rcpp::Rcout<<"currentHistoryPrior: "<<currentHistoryPrior<<std::endl;

    // Rcpp::Rcout<<"test1"<<std::endl;
    // Rcpp::Rcout<<"step6"<<std::endl;

    IntegerVector currentHistoryRetracing;
    if(vertexHistoryRetracing[agentNo] != R_NilValue){
      currentHistoryRetracing = vertexHistoryRetracing[agentNo];
    }
    // Rcpp::Rcout<<"step8"<<std::endl;

    // Rcpp::Rcout<<"test2"<<std::endl;

    //edges
    IntegerVector currentHistoryEdge;
    IntegerVector currentHistoryEdge_walk;
    IntegerVector currentHistoryEdge_dog;
    IntegerVector currentHistoryEdge_bike;
    IntegerVector currentHistoryEdge_jog;

    if(edgeHistory[agentNo] != R_NilValue){
      currentHistoryEdge = edgeHistory[agentNo];

      //remove duplicate values (agents are only counted once)
      currentHistoryEdge = unique(currentHistoryEdge);

      //also add history per agent type
      std::string agentTyp = agentTypes[agentNo];

      if(agentTyp == "walkNat" | agentTyp == "walkSoc" ){

        currentHistoryEdge_walk = edgeHistory[agentNo];
        //remove duplicate values (agents are only counted once)
        currentHistoryEdge_walk = unique(currentHistoryEdge_walk);

      }else if(agentTyp == "dogNat" | agentTyp == "dogProx" ){
        currentHistoryEdge_dog = edgeHistory[agentNo];
        //remove duplicate values (agents are only counted once)
        currentHistoryEdge_dog = unique(currentHistoryEdge_dog);

      }else if(agentTyp == "ebikeNat" | agentTyp == "bikeSport" ){
        currentHistoryEdge_bike = edgeHistory[agentNo];
        //remove duplicate values (agents are only counted once)
        currentHistoryEdge_bike = unique(currentHistoryEdge_bike);

      }else if(agentTyp == "jogger" ){
        currentHistoryEdge_jog = edgeHistory[agentNo];
        //remove duplicate values (agents are only counted once)
        currentHistoryEdge_jog = unique(currentHistoryEdge_jog);

      }

    }

    IntegerVector currentHistoryEdge_AOI;
    IntegerVector currentHistoryEdge_walk_AOI;
    IntegerVector currentHistoryEdge_dog_AOI;
    IntegerVector currentHistoryEdge_bike_AOI;
    IntegerVector currentHistoryEdge_jog_AOI;

    if(edgeHistoryAOI[agentNo] != R_NilValue){
      currentHistoryEdge_AOI = edgeHistoryAOI[agentNo];
      //remove duplicate values (agents are only counted once)
      currentHistoryEdge_AOI = unique(currentHistoryEdge_AOI);

      //also add history per agent type
      std::string agentTyp = agentTypes[agentNo];
      if(agentTyp == "walkNat" | agentTyp == "walkSoc" ){
        currentHistoryEdge_walk_AOI = edgeHistoryAOI[agentNo];
        //remove duplicate values (agents are only counted once)
        currentHistoryEdge_walk_AOI = unique(currentHistoryEdge_walk_AOI);

      }else if(agentTyp == "dogNat" | agentTyp == "dogProx" ){
        currentHistoryEdge_dog_AOI = edgeHistoryAOI[agentNo];
        //remove duplicate values (agents are only counted once)
        currentHistoryEdge_dog_AOI = unique(currentHistoryEdge_dog_AOI);

      }else if(agentTyp == "ebikeNat" | agentTyp == "bikeSport" ){
        currentHistoryEdge_bike_AOI = edgeHistoryAOI[agentNo];
        //remove duplicate values (agents are only counted once)
        currentHistoryEdge_bike_AOI = unique(currentHistoryEdge_bike_AOI);

      }else if(agentTyp == "jogger" ){
        currentHistoryEdge_jog_AOI = edgeHistoryAOI[agentNo];
        //remove duplicate values (agents are only counted once)
        currentHistoryEdge_jog_AOI = unique(currentHistoryEdge_jog_AOI);

      }
    }



    // Rcpp::Rcout<<"test3"<<std::endl;

    // Rcpp::Rcout<<"step9"<<std::endl;


    if(currentHistoryPrior.size() > 0){
      //cycle through each agent's prior vertex history
      for(int vrtx = 0; vrtx < currentHistoryPrior.size(); vrtx++ ){

        //determine nodeID
        int currentVertex = currentHistoryPrior[vrtx];

        //and translate nodeID presence in history, into passage
        vertexPassage[currentVertex] = vertexPassage[currentVertex] + 1;

        // Rcpp::Rcout<<"vertexPassage: "<<vertexPassage[currentVertex]<<std::endl;

      }
    }

    // Rcpp::Rcout<<"step5"<<std::endl;


    if(currentHistoryRetracing.size() > 0){
      //cycle through each agent's retracing vertex history
      for(int vrtx = 0; vrtx < currentHistoryRetracing.size(); vrtx++ ){


          //determine nodeID
          int currentVertex = currentHistoryRetracing[vrtx];

          //retracedV can be NULL if no retracing occurred
          //ignore if this is the case

          //and translate nodeID presence in history, into passage
          vertexPassage[currentVertex] = vertexPassage[currentVertex] + 1;

          // Rcpp::Rcout<<"vertexPassageRetracing: "<<vertexPassage[currentVertex]<<std::endl;
      }
    }

    // Rcpp::Rcout<<"step10"<<std::endl;

    //all
    if(currentHistoryEdge.size() > 0){
      //cycle through each agent's edge history
      for(int edg = 0; edg < currentHistoryEdge.size(); edg++ ){

          //determine nodeID
          int currentEdge = currentHistoryEdge[edg];

          //and translate nodeID presence in history, into passage
          edgePassage[currentEdge] = edgePassage[currentEdge] + 1;
          // Rcpp::Rcout<<"edgePassage1: "<<edgePassage[currentEdge]<<std::endl;

        }
      }
    if(currentHistoryEdge_AOI.size() > 0){
      //cycle through each agent's edge history
      for(int edg = 0; edg < currentHistoryEdge_AOI.size(); edg++ ){

        //determine nodeID
        int currentEdgeAOI = currentHistoryEdge_AOI[edg];

        //and translate nodeID presence in history, into passage
        edgePassageAOI[currentEdgeAOI] = edgePassageAOI[currentEdgeAOI] + 1;
        // Rcpp::Rcout<<"edgePassage2: "<<edgePassage[currentEdgeAOI]<<std::endl;

      }
    }
    //walk
    if(currentHistoryEdge_walk.size() > 0){
      //cycle through each agent's edge history
      for(int edg = 0; edg < currentHistoryEdge_walk.size(); edg++ ){

        //determine nodeID
        int currentEdge = currentHistoryEdge_walk[edg];

        //and translate nodeID presence in history, into passage
        edgePassageWalk[currentEdge] = edgePassageWalk[currentEdge] + 1;
        // Rcpp::Rcout<<"edgePassage1: "<<edgePassage[currentEdge]<<std::endl;

      }
    }
    if(currentHistoryEdge_walk_AOI.size() > 0){
      //cycle through each agent's edge history
      for(int edg = 0; edg < currentHistoryEdge_walk_AOI.size(); edg++ ){

        //determine nodeID
        int currentEdgeAOI = currentHistoryEdge_walk_AOI[edg];

        //and translate nodeID presence in history, into passage
        edgePassageWalkAOI[currentEdgeAOI] = edgePassageWalkAOI[currentEdgeAOI] + 1;
        // Rcpp::Rcout<<"edgePassage2: "<<edgePassage[currentEdgeAOI]<<std::endl;

      }
    }

    //dog
    if(currentHistoryEdge_dog.size() > 0){
      //cycle through each agent's edge history
      for(int edg = 0; edg < currentHistoryEdge_dog.size(); edg++ ){

        //determine nodeID
        int currentEdge = currentHistoryEdge_dog[edg];

        //and translate nodeID presence in history, into passage
        edgePassageDog[currentEdge] = edgePassageDog[currentEdge] + 1;
        // Rcpp::Rcout<<"edgePassage1: "<<edgePassage[currentEdge]<<std::endl;

      }
    }
    if(currentHistoryEdge_dog_AOI.size() > 0){
      //cycle through each agent's edge history
      for(int edg = 0; edg < currentHistoryEdge_dog_AOI.size(); edg++ ){

        //determine nodeID
        int currentEdgeAOI = currentHistoryEdge_dog_AOI[edg];

        //and translate nodeID presence in history, into passage
        edgePassageDogAOI[currentEdgeAOI] = edgePassageDogAOI[currentEdgeAOI] + 1;
        // Rcpp::Rcout<<"edgePassage2: "<<edgePassage[currentEdgeAOI]<<std::endl;

      }
    }

    //bike
    if(currentHistoryEdge_bike.size() > 0){
      //cycle through each agent's edge history
      for(int edg = 0; edg < currentHistoryEdge_bike.size(); edg++ ){

        //determine nodeID
        int currentEdge = currentHistoryEdge_bike[edg];

        //and translate nodeID presence in history, into passage
        edgePassageBike[currentEdge] = edgePassageBike[currentEdge] + 1;
        // Rcpp::Rcout<<"edgePassage1: "<<edgePassage[currentEdge]<<std::endl;

      }
    }
    if(currentHistoryEdge_bike_AOI.size() > 0){
      //cycle through each agent's edge history
      for(int edg = 0; edg < currentHistoryEdge_bike_AOI.size(); edg++ ){

        //determine nodeID
        int currentEdgeAOI = currentHistoryEdge_bike_AOI[edg];

        //and translate nodeID presence in history, into passage
        edgePassageBikeAOI[currentEdgeAOI] = edgePassageBikeAOI[currentEdgeAOI] + 1;
        // Rcpp::Rcout<<"edgePassage2: "<<edgePassage[currentEdgeAOI]<<std::endl;

      }
    }

    //jog
    if(currentHistoryEdge_jog.size() > 0){
      //cycle through each agent's edge history
      for(int edg = 0; edg < currentHistoryEdge_jog.size(); edg++ ){

        //determine nodeID
        int currentEdge = currentHistoryEdge_jog[edg];

        //and translate nodeID presence in history, into passage
        edgePassageJog[currentEdge] = edgePassageJog[currentEdge] + 1;
        // Rcpp::Rcout<<"edgePassage1: "<<edgePassage[currentEdge]<<std::endl;

      }
    }
    if(currentHistoryEdge_jog_AOI.size() > 0){
      //cycle through each agent's edge history
      for(int edg = 0; edg < currentHistoryEdge_jog_AOI.size(); edg++ ){

        //determine nodeID
        int currentEdgeAOI = currentHistoryEdge_jog_AOI[edg];

        //and translate nodeID presence in history, into passage
        edgePassageJogAOI[currentEdgeAOI] = edgePassageJogAOI[currentEdgeAOI] + 1;
        // Rcpp::Rcout<<"edgePassage2: "<<edgePassage[currentEdgeAOI]<<std::endl;

      }
    }
    // Rcpp::Rcout<<"FINISHED"<<std::endl;

    }

  // Rcpp::Rcout<<"step7"<<std::endl;


  // Rcpp::Rcout<<"step2"<<std::endl;

  // Rcpp::Rcout<<"FINISHED2"<<std::endl;

  //Pass passage container information into original passage vectors
  IntegerVector originalPassage_vertex = vertexTable["passage"];
  IntegerVector originalPassage_edge = edgeTable["passage"];
  IntegerVector originalPassage_edgeAOI = edgeTable["passageAOI"];
  IntegerVector originalPassageWalk_edge = edgeTable["passageWalk"];
  IntegerVector originalPassageWalk_edgeAOI = edgeTable["passageWalkAOI"];
  IntegerVector originalPassageDog_edge = edgeTable["passageDog"];
  IntegerVector originalPassageDog_edgeAOI = edgeTable["passageDogAOI"];
  IntegerVector originalPassageBike_edge = edgeTable["passageBike"];
  IntegerVector originalPassageBike_edgeAOI = edgeTable["passageBikeAOI"];
  IntegerVector originalPassageJog_edge = edgeTable["passageJog"];
  IntegerVector originalPassageJog_edgeAOI = edgeTable["passageJogAOI"];

  // Rcpp::Rcout<<"FINISHED3"<<std::endl;

  IntegerVector vertexIDs = vertexTable["nodeID"];
  IntegerVector edgeIDs = edgeTable["edgeID"];

  // Rcpp::Rcout<<"step11"<<std::endl;

  //cycle through vertexIDs and insert passage in relevant position
  for(int posV = 0; posV < vertexIDs.size() ; posV++){
    int thisV = vertexIDs[posV];
    int pssgPosV = originalPassage_vertex[posV];
    originalPassage_vertex[posV] = pssgPosV + vertexPassage[thisV];
  }

  // Rcpp::Rcout<<"step12"<<std::endl;

  //cycle through edgeIDs and insert passage in relevant position
  for(int posE = 0; posE < edgeIDs.size() ; posE++){
    int thisE = edgeIDs[posE];
    int pssgPosE = originalPassage_edge[posE];
    int pssgPosEAOI = originalPassage_edgeAOI[posE];
    int pssgPosEWalk = originalPassageWalk_edge[posE];
    int pssgPosEWalkAOI = originalPassageWalk_edgeAOI[posE];
    int pssgPosEDog = originalPassageDog_edge[posE];
    int pssgPosEDogAOI = originalPassageDog_edgeAOI[posE];
    int pssgPosEBike = originalPassageBike_edge[posE];
    int pssgPosEBikeAOI = originalPassageBike_edgeAOI[posE];
    int pssgPosEJog = originalPassageJog_edge[posE];
    int pssgPosEJogAOI = originalPassageJog_edgeAOI[posE];

//add both Passage and PassageAOI to _edge, _edgeAOI has only PassageAOI
    originalPassage_edge[posE] = pssgPosE + edgePassage[thisE] + edgePassageAOI[thisE];
    originalPassage_edgeAOI[posE] = pssgPosEAOI + edgePassageAOI[thisE];
    originalPassageWalk_edge[posE] = pssgPosEWalk + edgePassageWalk[thisE] + edgePassageWalkAOI[thisE];
    originalPassageWalk_edgeAOI[posE] = pssgPosEWalkAOI + edgePassageWalkAOI[thisE];
    originalPassageDog_edge[posE] = pssgPosEDog + edgePassageDog[thisE] + edgePassageDogAOI[thisE];
    originalPassageDog_edgeAOI[posE] = pssgPosEDogAOI + edgePassageDogAOI[thisE];
    originalPassageBike_edge[posE] = pssgPosEBike + edgePassageBike[thisE] + edgePassageBikeAOI[thisE];
    originalPassageBike_edgeAOI[posE] = pssgPosEBikeAOI + edgePassageBikeAOI[thisE];
    originalPassageJog_edge[posE] = pssgPosEJog + edgePassageJog[thisE] + edgePassageJogAOI[thisE];
    originalPassageJog_edgeAOI[posE] = pssgPosEJogAOI + edgePassageJogAOI[thisE];

  }

  return List::create(Named("vertex") = originalPassage_vertex, Named("edge") = originalPassage_edge, Named("edgeAOI") = originalPassage_edgeAOI,
                      Named("edgeWalk") = originalPassageWalk_edge,
                      Named("edgeWalkAOI") = originalPassageWalk_edgeAOI,
                      Named("edgeDog") = originalPassageDog_edge,
                      Named("edgeDogAOI") = originalPassageDog_edgeAOI,
                      Named("edgeBike") = originalPassageBike_edge,
                      Named("edgeBikeAOI") = originalPassageBike_edgeAOI,
                      Named("edgeJog") = originalPassageJog_edge,
                      Named("edgeJogAOI") = originalPassageJog_edgeAOI);
}



// #
// # for(agentNo in 1:nrow(dayPop) ){
// #   #for vertices
// #   history <- factor( dayPop$priorV[agentNo][[1]] )
// #   nodeIDs <- factor(V(network)$nodeID)
// #
// #   passageTable <- table(nodeIDs[match(history, nodeIDs)])
// #
// #
// #   V(network)$passage <- V(network)$passage + as.vector(passageTable)
// #
// #   #for edges
// #   history <- factor( dayPop$priorE[agentNo][[1]] )
// #   edgeIDs <- factor(E(network)$edgeID)
// #
// #   passageTable <- table(edgeIDs[match(history, edgeIDs)])
// #
// #
// #   E(network)$passage <- E(network)$passage + as.vector(passageTable)
// #
// # }


/*
 * appendCurrentToPrior:
 * takes List of priorV and vector of currentV, appends currentV to priorV and returns a List
 */

// [[Rcpp::export]]
List appendCurrentToPrior_cpp(List priorVs, List pathToGoal, IntegerVector currentVs, StringVector behaviour){

  List newList = List(priorVs.size());

  IntegerVector newVector;
// Rcpp::Rcout<<"step1"<<std::endl;
    for(int i = 0; i < priorVs.size(); i++){

      if(priorVs[i] == R_NilValue){

        //create a new vector for priorV
        // Rcpp::Rcout<<"CREATING new vector for priorV..."<<std::endl;

        if((behaviour[i] == "shortest") | (behaviour[i] == "nicestShortest") ){
          //create a vector with the pathToGoal
          // Rcpp::Rcout<<"...for SHORTEST behaviour"<<std::endl;

          newVector = IntegerVector(pathToGoal[i]);

        }else{
          //create a vector with currentV
          newVector = currentVs[i];
          // newVector[1] = currentVs[i];

          // Rcpp::Rcout<<"...for OTHER behaviour"<<std::endl;

        }

        // Rcpp::Rcout<<"step3"<<std::endl;

      }else{

        //append to existing vector for priorV
        // Rcpp::Rcout<<"step4"<<std::endl;

        IntegerVector priorV = priorVs[i];

        // Rcpp::Rcout<<"APPENDING vector for priorV..."<<std::endl;


        if((behaviour[i] == "shortest") | (behaviour[i] == "nicestShortest")){
          // Rcpp::Rcout<<"step5"<<std::endl;
          // Rcpp::Rcout<<"...for SHORTEST behaviour"<<std::endl;


          //append pathToGoal to existing priorVs
          IntegerVector thisPathToGoal = pathToGoal[i];

          newVector = IntegerVector(priorV.size()+thisPathToGoal.size());
          //copy over priorV elements
          for(int x = 0; x < priorV.size(); x++){
            newVector[x] = priorV[x];
          }
          //copy over thisPathToGoal elements
          for(int y = 0; y < thisPathToGoal.size(); y++){
            newVector[y+priorV.size()] = thisPathToGoal[y];
          }
          // Rcpp::Rcout<<"step6"<<std::endl;

        }else{
          // Rcpp::Rcout<<"step7"<<std::endl;
          // Rcpp::Rcout<<"...for OTHER behaviour"<<std::endl;

          //append currentV to existing priorVs
          int currentV = currentVs[i];

          newVector = IntegerVector(priorV.size()+1);

          for(int x = 0; x < priorV.size(); x++){
            newVector[x] = priorV[x];
          }
          //add currentV at END. This is equivalent to size of vector (not like R)
          newVector[priorV.size()] = currentV;
        }
        // Rcpp::Rcout<<"step8"<<std::endl;

      }
      // Rcpp::Rcout<<"step9"<<std::endl;

      newList[i] = newVector;

      // Rcpp::Rcout<<"***********************"<<std::endl;

    }

  return newList;

}


/*
 * generateAdjListAndDistTbl:
 * takes DataFrames edgeTable and nodeTable,
 * returns adjacency lists and distance table to be used in shortest path function
 */

// [[Rcpp::export]]
List generateAdjListAndDistTbl_cpp(DataFrame edgeTable, DataFrame vertexTable){


  // High vertex ID (allows to allocate based on ID ((ID 0 doesnt exist but position 0 will exist but not hold anything)))
  IntegerVector nodeIDs = vertexTable["nodeID"];

  int V = Rcpp::max(nodeIDs);

////// CREATE ADJACENCY LIST ///////////
  //create adjacency list of IDs
  std::vector<std::vector<int>> adjList_IDs = std::vector<std::vector<int>> (V+1);
  //create adjacency list of distances
  std::vector<std::vector<double>> adjList_dist_walkNat = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_walkNat_attr = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_walkNat_ATTR = std::vector<std::vector<double>>(V+1);

  std::vector<std::vector<double>> adjList_dist_walkSoc = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_walkSoc_attr = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_walkSoc_ATTR = std::vector<std::vector<double>>(V+1);

  std::vector<std::vector<double>> adjList_dist_dogNat = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_dogNat_attr = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_dogNat_ATTR = std::vector<std::vector<double>>(V+1);

  std::vector<std::vector<double>> adjList_dist_dogProx = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_dogProx_attr = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_dogProx_ATTR = std::vector<std::vector<double>>(V+1);

  std::vector<std::vector<double>> adjList_dist_ebikeNat = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_ebikeNat_attr = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_ebikeNat_ATTR = std::vector<std::vector<double>>(V+1);

  std::vector<std::vector<double>> adjList_dist_jogger = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_jogger_attr = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_jogger_ATTR = std::vector<std::vector<double>>(V+1);

  std::vector<std::vector<double>> adjList_dist_bikeSport = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_bikeSport_attr = std::vector<std::vector<double>>(V+1);
  std::vector<std::vector<double>> adjList_dist_bikeSport_ATTR = std::vector<std::vector<double>>(V+1);

  //create adjacency list of distances to goal
  // std::vector<std::vector<double>> adjList_totDistGoal = std::vector<std::vector<double>>(V+1);


  NumericVector edge_weights_walkNat = edgeTable["SHAPE_Leng_walkNat"];
  NumericVector edge_weights_walkNat_attr = edgeTable["SHAPE_Leng_walkNat_attr"];
  NumericVector edge_weights_walkNat_ATTR = edgeTable["SHAPE_Leng_walkNat_ATTR"];

  Rcpp::Rcout<<"edge_weights_walkNat[1]: "<<edge_weights_walkNat[1]<<std::endl;

  NumericVector edge_weights_walkSoc = edgeTable["SHAPE_Leng_walkSoc"];
  NumericVector edge_weights_walkSoc_attr = edgeTable["SHAPE_Leng_walkSoc_attr"];
  NumericVector edge_weights_walkSoc_ATTR = edgeTable["SHAPE_Leng_walkSoc_ATTR"];

  NumericVector edge_weights_dogNat = edgeTable["SHAPE_Leng_dogNat"];
  NumericVector edge_weights_dogNat_attr = edgeTable["SHAPE_Leng_dogNat_attr"];
  NumericVector edge_weights_dogNat_ATTR = edgeTable["SHAPE_Leng_dogNat_ATTR"];

  NumericVector edge_weights_dogProx = edgeTable["SHAPE_Leng_dogProx"];
  NumericVector edge_weights_dogProx_attr = edgeTable["SHAPE_Leng_dogProx_attr"];
  NumericVector edge_weights_dogProx_ATTR = edgeTable["SHAPE_Leng_dogProx_ATTR"];

  NumericVector edge_weights_ebikeNat = edgeTable["SHAPE_Leng_ebikeNat"];
  NumericVector edge_weights_ebikeNat_attr = edgeTable["SHAPE_Leng_ebikeNat_attr"];
  NumericVector edge_weights_ebikeNat_ATTR = edgeTable["SHAPE_Leng_ebikeNat_ATTR"];

  NumericVector edge_weights_bikeSport = edgeTable["SHAPE_Leng_bikeSport"];
  NumericVector edge_weights_bikeSport_attr = edgeTable["SHAPE_Leng_bikeSport_attr"];
  NumericVector edge_weights_bikeSport_ATTR = edgeTable["SHAPE_Leng_bikeSport_ATTR"];

  NumericVector edge_weights_jogger = edgeTable["SHAPE_Leng_jogger"];
  NumericVector edge_weights_jogger_attr = edgeTable["SHAPE_Leng_jogger_attr"];
  NumericVector edge_weights_jogger_ATTR = edgeTable["SHAPE_Leng_jogger_ATTR"];


  //TODO:
  //Generate other adacency lists using different weights than pure distance

  //prepare to and from vectors
  IntegerVector v_to = edgeTable["to"];
  IntegerVector v_from = edgeTable["from"];

  // Single O(E) pass instead of O(V*E) nested loops.
  // For each edge, add both directions (undirected graph).
  for(int edgeRow = 0; edgeRow < v_to.size(); edgeRow++){
    int toNode   = v_to[edgeRow];
    int fromNode = v_from[edgeRow];

    // to -> from direction
    adjList_IDs[toNode].push_back(fromNode);
    adjList_dist_walkNat[toNode].push_back(edge_weights_walkNat[edgeRow]);
    adjList_dist_walkNat_attr[toNode].push_back(edge_weights_walkNat_attr[edgeRow]);
    adjList_dist_walkNat_ATTR[toNode].push_back(edge_weights_walkNat_ATTR[edgeRow]);
    adjList_dist_walkSoc[toNode].push_back(edge_weights_walkSoc[edgeRow]);
    adjList_dist_walkSoc_attr[toNode].push_back(edge_weights_walkSoc_attr[edgeRow]);
    adjList_dist_walkSoc_ATTR[toNode].push_back(edge_weights_walkSoc_ATTR[edgeRow]);
    adjList_dist_dogNat[toNode].push_back(edge_weights_dogNat[edgeRow]);
    adjList_dist_dogNat_attr[toNode].push_back(edge_weights_dogNat_attr[edgeRow]);
    adjList_dist_dogNat_ATTR[toNode].push_back(edge_weights_dogNat_ATTR[edgeRow]);
    adjList_dist_dogProx[toNode].push_back(edge_weights_dogProx[edgeRow]);
    adjList_dist_dogProx_attr[toNode].push_back(edge_weights_dogProx_attr[edgeRow]);
    adjList_dist_dogProx_ATTR[toNode].push_back(edge_weights_dogProx_ATTR[edgeRow]);
    adjList_dist_ebikeNat[toNode].push_back(edge_weights_ebikeNat[edgeRow]);
    adjList_dist_ebikeNat_attr[toNode].push_back(edge_weights_ebikeNat_attr[edgeRow]);
    adjList_dist_ebikeNat_ATTR[toNode].push_back(edge_weights_ebikeNat_ATTR[edgeRow]);
    adjList_dist_bikeSport[toNode].push_back(edge_weights_bikeSport[edgeRow]);
    adjList_dist_bikeSport_attr[toNode].push_back(edge_weights_bikeSport_attr[edgeRow]);
    adjList_dist_bikeSport_ATTR[toNode].push_back(edge_weights_bikeSport_ATTR[edgeRow]);
    adjList_dist_jogger[toNode].push_back(edge_weights_jogger[edgeRow]);
    adjList_dist_jogger_attr[toNode].push_back(edge_weights_jogger_attr[edgeRow]);
    adjList_dist_jogger_ATTR[toNode].push_back(edge_weights_jogger_ATTR[edgeRow]);

    // from -> to direction
    adjList_IDs[fromNode].push_back(toNode);
    adjList_dist_walkNat[fromNode].push_back(edge_weights_walkNat[edgeRow]);
    adjList_dist_walkNat_attr[fromNode].push_back(edge_weights_walkNat_attr[edgeRow]);
    adjList_dist_walkNat_ATTR[fromNode].push_back(edge_weights_walkNat_ATTR[edgeRow]);
    adjList_dist_walkSoc[fromNode].push_back(edge_weights_walkSoc[edgeRow]);
    adjList_dist_walkSoc_attr[fromNode].push_back(edge_weights_walkSoc_attr[edgeRow]);
    adjList_dist_walkSoc_ATTR[fromNode].push_back(edge_weights_walkSoc_ATTR[edgeRow]);
    adjList_dist_dogNat[fromNode].push_back(edge_weights_dogNat[edgeRow]);
    adjList_dist_dogNat_attr[fromNode].push_back(edge_weights_dogNat_attr[edgeRow]);
    adjList_dist_dogNat_ATTR[fromNode].push_back(edge_weights_dogNat_ATTR[edgeRow]);
    adjList_dist_dogProx[fromNode].push_back(edge_weights_dogProx[edgeRow]);
    adjList_dist_dogProx_attr[fromNode].push_back(edge_weights_dogProx_attr[edgeRow]);
    adjList_dist_dogProx_ATTR[fromNode].push_back(edge_weights_dogProx_ATTR[edgeRow]);
    adjList_dist_ebikeNat[fromNode].push_back(edge_weights_ebikeNat[edgeRow]);
    adjList_dist_ebikeNat_attr[fromNode].push_back(edge_weights_ebikeNat_attr[edgeRow]);
    adjList_dist_ebikeNat_ATTR[fromNode].push_back(edge_weights_ebikeNat_ATTR[edgeRow]);
    adjList_dist_bikeSport[fromNode].push_back(edge_weights_bikeSport[edgeRow]);
    adjList_dist_bikeSport_attr[fromNode].push_back(edge_weights_bikeSport_attr[edgeRow]);
    adjList_dist_bikeSport_ATTR[fromNode].push_back(edge_weights_bikeSport_ATTR[edgeRow]);
    adjList_dist_jogger[fromNode].push_back(edge_weights_jogger[edgeRow]);
    adjList_dist_jogger_attr[fromNode].push_back(edge_weights_jogger_attr[edgeRow]);
    adjList_dist_jogger_ATTR[fromNode].push_back(edge_weights_jogger_ATTR[edgeRow]);
  }

// Ignore for now: only useful for A*, for now Djkistra is fast enough

// ///// CREATE PRECALCULATED TABLE OF DISTANCES BETWEEN EVERY NODE PAIR ////////
//   //calculate a graph that determines distances from every vertex to every other vertex
//   // V+1 vectors of V+1 vectors
//   std::vector<std::vector<double>> allDistancesTbl(V+1, std::vector<double>(V+1));
//   NumericVector lat_v = vertexTable["lat"];
//   NumericVector lon_v = vertexTable["lon"];
//
//   double totDist;
//   for(int vrt1 = 1; vrt1 < V+1; vrt1++){
//
//     for(int vrt2 = 1; vrt2 < V+1; vrt2++){
//
//       if(vrt1 != vrt2){
//
//         //calculate straight line distance between two vertices
//         // and the total distance (in m) from goal vector ( x = lat, y = lon)
//         double x1 = lat_v[vrt1-1] ;
//         double x2 = lat_v[vrt2-1] ;
//         // longitude in meters depends on latitude following eq:
//         double y1 = lon_v[vrt1-1] ;
//         double y2 = lon_v[vrt2-1] ;
//         //get distance from vertex (x1, y1) to goal (x2, y2) (convert to meters afterwards)
//         totDist = std::sqrt(pow( (x2 - x1)* 111.32, 2 ) + pow((y2-y1)*(40075 * std::cos( lat_v[vrt1] ) / 360), 2) );
//         totDist = totDist * 1000;
//
//
//         // Rcpp::Rcout<<"totDist: "<<totDist<<std::endl;
//
//         //add it to table of all distances
//         allDistancesTbl[vrt1][vrt2] = totDist;
//
//       }
//
//     }
//
//   }


  //assign pointers
  // int* V_ptr = new int(V);
  // std::vector<std::vector<double>>* adjListDist_walkNat_ptr = new std::vector<std::vector<double>>(adjList_dist_walkNat);
  // std::vector<std::vector<double>>* adjListDist_walkNat_attr_ptr = new std::vector<std::vector<double>>(adjList_dist_walkNat_attr);
  // std::vector<std::vector<double>>* adjListDist_walkNat_ATTR_ptr = new std::vector<std::vector<double>>(adjList_dist_walkNat_ATTR);
  //
  // std::vector<std::vector<double>>* adjListDist_walkSoc_ptr = new std::vector<std::vector<double>>(adjList_dist_walkSoc);
  // std::vector<std::vector<double>>* adjListDist_walkSoc_attr_ptr = new std::vector<std::vector<double>>(adjList_dist_walkSoc_attr);
  // std::vector<std::vector<double>>* adjListDist_walkSoc_ATTR_ptr = new std::vector<std::vector<double>>(adjList_dist_walkSoc_ATTR);
  //
  // std::vector<std::vector<double>>* adjListDist_dogNat_ptr = new std::vector<std::vector<double>>(adjList_dist_dogNat);
  // std::vector<std::vector<double>>* adjListDist_dogNat_attr_ptr = new std::vector<std::vector<double>>(adjList_dist_dogNat_attr);
  // std::vector<std::vector<double>>* adjListDist_dogNat_ATTR_ptr = new std::vector<std::vector<double>>(adjList_dist_dogNat_ATTR);
  //
  // std::vector<std::vector<double>>* adjListDist_dogProx_ptr = new std::vector<std::vector<double>>(adjList_dist_dogProx);
  // std::vector<std::vector<double>>* adjListDist_dogProx_attr_ptr = new std::vector<std::vector<double>>(adjList_dist_dogProx_attr);
  // std::vector<std::vector<double>>* adjListDist_dogProx_ATTR_ptr = new std::vector<std::vector<double>>(adjList_dist_dogProx_ATTR);
  //
  // std::vector<std::vector<double>>* adjListDist_ebikeNat_ptr = new std::vector<std::vector<double>>(adjList_dist_ebikeNat);
  // std::vector<std::vector<double>>* adjListDist_ebikeNat_attr_ptr = new std::vector<std::vector<double>>(adjList_dist_ebikeNat_attr);
  // std::vector<std::vector<double>>* adjListDist_ebikeNat_ATTR_ptr = new std::vector<std::vector<double>>(adjList_dist_ebikeNat_ATTR);
  //
  // std::vector<std::vector<double>>* adjListDist_bikeSport_ptr = new std::vector<std::vector<double>>(adjList_dist_bikeSport);
  // std::vector<std::vector<double>>* adjListDist_bikeSport_attr_ptr = new std::vector<std::vector<double>>(adjList_dist_bikeSport_attr);
  // std::vector<std::vector<double>>* adjListDist_bikeSport_ATTR_ptr = new std::vector<std::vector<double>>(adjList_dist_bikeSport_ATTR);
  //
  // std::vector<std::vector<double>>* adjListDist_jogger_ptr = new std::vector<std::vector<double>>(adjList_dist_jogger);
  // std::vector<std::vector<double>>* adjListDist_jogger_attr_ptr = new std::vector<std::vector<double>>(adjList_dist_jogger_attr);
  // std::vector<std::vector<double>>* adjListDist_jogger_ATTR_ptr = new std::vector<std::vector<double>>(adjList_dist_jogger_ATTR);
  //
  // std::vector<std::vector<int>>* adjListIDs_ptr = new std::vector<std::vector<int>>(adjList_IDs);
  // // std::vector<std::vector<double>>* allDistTbl_ptr = new std::vector<std::vector<double>>(allDistancesTbl);
  //
  // //wrap pointers into Xptr class for R
  // Rcpp::XPtr< int > Vptr(V_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_walkNat(adjListDist_walkNat_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_walkNat_attr(adjListDist_walkNat_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_walkNat_ATTR(adjListDist_walkNat_ptr, true);
  //
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_walkSoc(adjListDist_walkSoc_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_walkSoc_attr(adjListDist_walkSoc_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_walkSoc_ATTR(adjListDist_walkSoc_ptr, true);
  //
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_dogNat(adjListDist_dogNat_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_dogNat_attr(adjListDist_dogNat_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_dogNat_ATTR(adjListDist_dogNat_ptr, true);
  //
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_dogProx(adjListDist_dogProx_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_dogProx_attr(adjListDist_dogProx_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_dogProx_ATTR(adjListDist_dogProx_ptr, true);
  //
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_ebikeNat(adjListDist_ebikeNat_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_ebikeNat_attr(adjListDist_ebikeNat_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_ebikeNat_ATTR(adjListDist_ebikeNat_ptr, true);
  //
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_bikeSport(adjListDist_bikeSport_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_bikeSport_attr(adjListDist_bikeSport_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_bikeSport_ATTR(adjListDist_bikeSport_ptr, true);
  //
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_jogger(adjListDist_jogger_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_jogger_attr(adjListDist_jogger_ptr, true);
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjListDistPtr_jogger_ATTR(adjListDist_jogger_ptr, true);
  //
  // Rcpp::XPtr< std::vector<std::vector<int>> > adjListIDsPtr(adjListIDs_ptr, true);
  // // Rcpp::XPtr< std::vector<std::vector<double>> > allDistTblPtr(allDistTbl_ptr);
  //
  // //return list of pointers
  // return  List::create(Vptr,  adjListIDsPtr,
  //                      List::create(adjListDistPtr_walkNat, adjListDistPtr_walkSoc, adjListDistPtr_dogNat,
  //                                   adjListDistPtr_dogProx, adjListDistPtr_ebikeNat, adjListDistPtr_bikeSport,
  //                                   adjListDistPtr_jogger),
  //                     List::create(adjListDistPtr_walkNat_attr, adjListDistPtr_walkSoc_attr,  adjListDistPtr_dogNat_attr,
  //                                  adjListDistPtr_dogProx_attr, adjListDistPtr_ebikeNat_attr, adjListDistPtr_bikeSport_attr,
  //                                  adjListDistPtr_jogger_attr),
  //                      List::create( adjListDistPtr_walkNat_ATTR,  adjListDistPtr_walkSoc_ATTR, adjListDistPtr_dogNat_ATTR,
  //                                    adjListDistPtr_dogProx_ATTR, adjListDistPtr_ebikeNat_ATTR, adjListDistPtr_bikeSport_ATTR,
  //                                    adjListDistPtr_jogger_ATTR)
  //
  //                      ); //, allDistTblPtr

  //return objects (not pointers), might be much slower
  return  List::create(V,  adjList_IDs,
                       List::create(adjList_dist_walkNat, adjList_dist_walkSoc, adjList_dist_dogNat,
                                    adjList_dist_dogProx, adjList_dist_ebikeNat, adjList_dist_bikeSport,
                                    adjList_dist_jogger),
                                    List::create(adjList_dist_walkNat_attr, adjList_dist_walkSoc_attr,  adjList_dist_dogNat_attr,
                                                 adjList_dist_dogProx_attr, adjList_dist_ebikeNat_attr, adjList_dist_bikeSport_attr,
                                                 adjList_dist_jogger_attr),
                                                 List::create( adjList_dist_walkNat_ATTR,  adjList_dist_walkSoc_ATTR, adjList_dist_dogNat_ATTR,
                                                               adjList_dist_dogProx_ATTR, adjList_dist_ebikeNat_ATTR, adjList_dist_bikeSport_ATTR,
                                                               adjList_dist_jogger_ATTR)

  );
  //, adjListDistPtr_walkNat_attr, adjListDistPtr_walkNat_ATTR,
  // ,
  //
  // ,
  // ,
  //

}





/*
 * findShortestRoute:
 * takes DataFrames edgeTable and nodeTable, as well as a string to indicate how distance is weighed.
 * returns Vector of node IDs of shortest route found via Djikstra's Algorithm
 */

// [[Rcpp::export]]
List findShortestRoute_cpp( int V_ptr,
                            std::vector<std::vector<int>> adjList_IDs_ptr,
                            std::vector<std::vector<double>> adjList_dist_ptr_walkNat,
                            std::vector<std::vector<double>> adjList_dist_ptr_walkNat_attr,
                            std::vector<std::vector<double>> adjList_dist_ptr_walkNat_ATTR,
                            std::vector<std::vector<double>> adjList_dist_ptr_walkSoc,
                            std::vector<std::vector<double>> adjList_dist_ptr_walkSoc_attr,
                            std::vector<std::vector<double>> adjList_dist_ptr_walkSoc_ATTR,
                            std::vector<std::vector<double>> adjList_dist_ptr_dogNat,
                            std::vector<std::vector<double>> adjList_dist_ptr_dogNat_attr,
                            std::vector<std::vector<double>> adjList_dist_ptr_dogNat_ATTR,
                            std::vector<std::vector<double>> adjList_dist_ptr_dogProx,
                            std::vector<std::vector<double>> adjList_dist_ptr_dogProx_attr,
                            std::vector<std::vector<double>> adjList_dist_ptr_dogProx_ATTR,
                            std::vector<std::vector<double>> adjList_dist_ptr_ebikeNat,
                            std::vector<std::vector<double>> adjList_dist_ptr_ebikeNat_attr,
                            std::vector<std::vector<double>> adjList_dist_ptr_ebikeNat_ATTR,
                            std::vector<std::vector<double>> adjList_dist_ptr_bikeSport,
                            std::vector<std::vector<double>> adjList_dist_ptr_bikeSport_attr,
                            std::vector<std::vector<double>> adjList_dist_ptr_bikeSport_ATTR,
                            std::vector<std::vector<double>> adjList_dist_ptr_jogger,
                            std::vector<std::vector<double>> adjList_dist_ptr_jogger_attr,
                            std::vector<std::vector<double>> adjList_dist_ptr_jogger_ATTR,
                           String weighingMethod,
                           std::vector<int> src_v,
                           std::vector<int> goal_v,
                           std::vector<std::string> agentTyps){// Rcpp::XPtr< std::vector<std::vector<double>> > allDistTbl_ptr,

  //removed:
  //Rcpp::XPtr<  >

  //retrieve objects from R that were generated in C++ earlier
  int V = V_ptr;

  // Select per-weighingMethod adjacency lists (one copy per call, not per agent)
  const std::vector<std::vector<double>>* adj_walkNat;
  const std::vector<std::vector<double>>* adj_walkSoc;
  const std::vector<std::vector<double>>* adj_dogNat;
  const std::vector<std::vector<double>>* adj_dogProx;
  const std::vector<std::vector<double>>* adj_ebikeNat;
  const std::vector<std::vector<double>>* adj_bikeSport;
  const std::vector<std::vector<double>>* adj_jogger;

  if(weighingMethod == "distance"){
    adj_walkNat   = &adjList_dist_ptr_walkNat;
    adj_walkSoc   = &adjList_dist_ptr_walkSoc;
    adj_dogNat    = &adjList_dist_ptr_dogNat;
    adj_dogProx   = &adjList_dist_ptr_dogProx;
    adj_ebikeNat  = &adjList_dist_ptr_ebikeNat;
    adj_bikeSport = &adjList_dist_ptr_bikeSport;
    adj_jogger    = &adjList_dist_ptr_jogger;
  }else if(weighingMethod == "little_attr"){
    adj_walkNat   = &adjList_dist_ptr_walkNat_attr;
    adj_walkSoc   = &adjList_dist_ptr_walkSoc_attr;
    adj_dogNat    = &adjList_dist_ptr_dogNat_attr;
    adj_dogProx   = &adjList_dist_ptr_dogProx_attr;
    adj_ebikeNat  = &adjList_dist_ptr_ebikeNat_attr;
    adj_bikeSport = &adjList_dist_ptr_bikeSport_attr;
    adj_jogger    = &adjList_dist_ptr_jogger_attr;
  }else{
    adj_walkNat   = &adjList_dist_ptr_walkNat_ATTR;
    adj_walkSoc   = &adjList_dist_ptr_walkSoc_ATTR;
    adj_dogNat    = &adjList_dist_ptr_dogNat_ATTR;
    adj_dogProx   = &adjList_dist_ptr_dogProx_ATTR;
    adj_ebikeNat  = &adjList_dist_ptr_ebikeNat_ATTR;
    adj_bikeSport = &adjList_dist_ptr_bikeSport_ATTR;
    adj_jogger    = &adjList_dist_ptr_jogger_ATTR;
  }
  // Use const ref to avoid copying the ID list
  const std::vector<std::vector<int>>& adjList_IDs = adjList_IDs_ptr;


  // create original variables used in pathfinding
  typedef std::pair<double, int> pqPair;

  //vector holding total distances to every vertex from source
  std::vector<double> dist_o = std::vector<double>(V+1, 100000000.0);

  //vector holding the prior vertex that lead to each vertex
  //this allows to gather all vertices after arriving at goal by stepping backwards
  std::vector<int> pathSteps_o = std::vector<int>(V+1, 100000000);

  //prepare output container for every agent
  std::vector<std::vector<int>> outputPath = std::vector<std::vector<int>>(src_v.size());
  std::vector<double> outputDist = std::vector<double>(src_v.size());

  // START EVALUATING DISTANCES FOR EVERY SOURCE-GOAL pair
  for(int agentNo = 0; agentNo < src_v.size(); agentNo++){

    //get agent type
    std::string agentType = agentTyps[agentNo];

    // Use pointer to pre-selected adjacency list — no per-agent copy
    const std::vector<std::vector<double>>* adjList_dist;

    if(agentType == "walkNat"){
      adjList_dist = adj_walkNat;
    }else if(agentType == "walkSoc"){
      adjList_dist = adj_walkSoc;
    }else if(agentType == "dogNat"){
      adjList_dist = adj_dogNat;
    }else if(agentType == "dogProx"){
      adjList_dist = adj_dogProx;
    }else if(agentType == "ebikeNat"){
      adjList_dist = adj_ebikeNat;
    }else if(agentType == "bikeSport"){
      adjList_dist = adj_bikeSport;
    }else{
      adjList_dist = adj_jogger;
    }


    //initialize a priority queue
    std::priority_queue<pqPair, std::vector<pqPair>, std::greater<pqPair>> pq;

    //refresh variables for each agent
    int src = src_v[agentNo];
    int goal = goal_v[agentNo];
    std::vector<double> dist(dist_o);
    std::vector<int> pathSteps(pathSteps_o);

    //if src and goal are the same, make a path with the goal vertex
    if(src == goal){
      std::vector<int> goal_v;
      goal_v.insert(goal_v.begin(), goal);
      outputPath[agentNo] = goal_v;
      outputDist[agentNo] = 0;

    }else{//otherwise, find shortest path

      //start with first node (src)
      // Distance of source vertex from itself is always 0
      pq.push(std::make_pair(0.0, src));
      dist[src] = 0.0;


      // Find shortest path for all vertices
      while(!pq.empty()) {

        //get vertex with shortest total distance from pq (sptv)
        int sptv = pq.top().second;
        pq.pop();

        //IF GOAL IS REACHED
        if(sptv == goal){
          //create path container
          std::vector<int> path;
          //step backwards through pathSteps, and note path
          int vrtx = goal;
          while(vrtx != src){
            // Rcpp::Rcout<<"vrtx: "<<vrtx<<std::endl;
            //
            if(vrtx != 100000000){
              path.insert(path.begin(), vrtx);
              vrtx = pathSteps[vrtx];
            }else{
              Rprintf("ERROR::: wrong nodeID retrieved (1000000)");
              break;
            }
          }

          //Save path and distance for this agent
          // return List::create(Named("path")= wrap(path), Named("distance") = wrap(dist[min_vrt]) );
          // path.pop_back();
          outputPath[agentNo] =  path;//remove last vertex (startV)
          outputDist[agentNo] = dist[sptv];

          //break out of two loops to start new agentloop
          goto endAgentLoop;
        }

        //determine relevant adjacency vertices and distances (const refs — no copy)
        const std::vector<int>&    adjVrts = adjList_IDs[sptv];
        const std::vector<double>& adjDist = (*adjList_dist)[sptv];


        //CYCLE ALL ADJACENT VERTICES
        //if needed, update their total distances (dist) and prior vertex
        for(int vrt = 0; vrt < adjVrts.size(); vrt++){
          int adjv = adjVrts[vrt];

          //determine distance between adjacent vertices
          double adjd = adjDist[vrt];


          //if smaller
          //distance is distance to prior vertex (sptv) with distance between sptv and this vertex (adjv)
          if(dist[adjv] > dist[sptv] + adjd){
            dist[adjv] = dist[sptv] + adjd;
            //record vertex that had shortest path to adjv
            pathSteps[adjv] = sptv;
            //return pair into pq
            pq.push(std::make_pair(dist[adjv], adjv));

          }

        }

      }
      endAgentLoop:
        ;
      //end of loop for each agent
    }
  }

  return List::create(Named("path") = outputPath, Named("distance") = outputDist);
}




/*
 * findClosestAOI:
 * takes DataFrames edgeTable and nodeTable, as well as a string to indicate how distance is weighed.
 * returns Vector of node IDs of shortest route found via Djikstra's Algorithm
 */

// [[Rcpp::export]]
List findClosestAOI_cpp(std::vector<std::string> AOIList_o,
                        std::vector<std::string> AOI_v,
                         int V_ptr,
                        std::vector<std::vector<int>> adjList_IDs_ptr,
                         std::vector<std::vector<double>> adjList_dist_ptr_walkNat,
                         std::vector<std::vector<double>> adjList_dist_ptr_walkSoc,
                         std::vector<std::vector<double>> adjList_dist_ptr_dogNat,
                         std::vector<std::vector<double>> adjList_dist_ptr_dogProx,
                         std::vector<std::vector<double>> adjList_dist_ptr_ebikeNat,
                         std::vector<std::vector<double>> adjList_dist_ptr_bikeSport,
                         std::vector<std::vector<double>> adjList_dist_ptr_jogger,
                        std::vector<int> src_v,
                        std::vector<double> agentSpeeds,
                        std::vector<double> agentDurations,
                        std::vector<std::string> AOI_aois,
                        std::vector<double> AOI_dulns,
                        std::vector<std::string> agentTyps,
                        std::vector<double> AOI_areas){// Rcpp::XPtr< std::vector<std::vector<double>> > allDistTbl_ptr,

std::ofstream log_file( "C:/Users/frueh/Documents/visitorFlowTool_LOG/rcpp_log.txt", std::ios_base::app);


  //removed things to make it non-pointers
  //Rcpp::XPtr<

  // Rcpp::XPtr< std::vector<std::vector<double>> > adjList_dist_ptr_walkSoc,
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjList_dist_ptr_dogNat,
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjList_dist_ptr_dogProx,
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjList_dist_ptr_ebikeNat,
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjList_dist_ptr_bikeSport,
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjList_dist_ptr_jogger,

  //retrieve objects from R that were generated in C++ earlier
  int V = V_ptr;
  ///////*
  ///////removed ptr assignement (replaced with simple assignement)

  log_file << "step1" << std::endl;

  //unwrap nested list of pointers (only take first of of dist pointers ([4]), others (attr and ATTR aren't used here))
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjList_dist_ptr_walkNat = adjList_dist_ptrs[1];
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjList_dist_ptr_walkSoc = adjList_dist_ptrs[2];
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjList_dist_ptr_dogNat = adjList_dist_ptrs[3];
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjList_dist_ptr_dogProx = adjList_dist_ptrs[4];
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjList_dist_ptr_ebikeNat = adjList_dist_ptrs[5];
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjList_dist_ptr_bikeSport = adjList_dist_ptrs[6];
  // Rcpp::XPtr< std::vector<std::vector<double>> > adjList_dist_ptr_jogger = adjList_dist_ptrs[7];


  // Use const refs to avoid unnecessary deep copies of large adjacency lists
  const std::vector<std::vector<double>>& adjList_dist_walkNat   = adjList_dist_ptr_walkNat;
  const std::vector<std::vector<double>>& adjList_dist_walkSoc   = adjList_dist_ptr_walkSoc;
  const std::vector<std::vector<double>>& adjList_dist_dogNat    = adjList_dist_ptr_dogNat;
  const std::vector<std::vector<double>>& adjList_dist_dogProx   = adjList_dist_ptr_dogProx;
  const std::vector<std::vector<double>>& adjList_dist_ebikeNat  = adjList_dist_ptr_ebikeNat;
  const std::vector<std::vector<double>>& adjList_dist_bikeSport = adjList_dist_ptr_bikeSport;
  const std::vector<std::vector<double>>& adjList_dist_jogger    = adjList_dist_ptr_jogger;



  const std::vector<std::vector<int>>& adjList_IDs = adjList_IDs_ptr;
  // std::vector<std::vector<double>> allDistancesTbl = *allDistTbl_ptr;

  log_file << "step2" << std::endl;

  // create original variables used in pathfinding
  typedef std::pair<double, int> pqPair;

  //vector holding total distances to every vertex from source
  std::vector<double> dist_o = std::vector<double>(V+1, 100000000.0);

  //vector holding the prior vertex that lead to each vertex
  //this allows to gather all vertices after arriving at goal by stepping backwards
  std::vector<int> pathSteps_o = std::vector<int>(V+1, 100000000);

  //prepare output containers for every agent
  //distances to AOI node, nodeID of AOI node, and AOI identifier ("A", "B" etc.)
  std::vector<std::vector<double>> outputDist =  std::vector<std::vector<double>>(src_v.size());
  std::vector<std::vector<int>> outputNode =  std::vector<std::vector<int>>(src_v.size());
  std::vector<std::vector<std::string>> outputAOI =  std::vector<std::vector<std::string>>(src_v.size());

  std::vector<std::vector<double>> outputProb = std::vector<std::vector<double>>(src_v.size());

  //prepare vector of AOIs
  // std::vector<char> AOI_v = vertexTable["AOICol"];
  // char firstline = '0';
  AOI_v.insert(AOI_v.begin(), "0"); //add element to front so node 1 is on position 1 of vector (rather than pos 0)


  log_file << "step3" << std::endl;

  // START EVALUATING DISTANCES FOR EVERY SOURCE-GOAL pair
  for(int agentNo = 0; agentNo < src_v.size(); agentNo++){

    log_file << "step4: agent: " << agentNo << std::endl;

    //get agent type
    std::string agentType = agentTyps[agentNo];

    // Use pointer to const ref — no per-agent copy of large adjacency list
    const std::vector<std::vector<double>>* adjList_dist;
    if(agentType == "walkNat"){
      adjList_dist = &adjList_dist_walkNat;
    }else if(agentType == "walkSoc"){
      adjList_dist = &adjList_dist_walkSoc;
    }else if(agentType == "dogNat"){
      adjList_dist = &adjList_dist_dogNat;
    }else if(agentType == "dogProx"){
      adjList_dist = &adjList_dist_dogProx;
    }else if(agentType == "ebikeNat"){
      adjList_dist = &adjList_dist_ebikeNat;
    }else if(agentType == "bikeSport"){
      adjList_dist = &adjList_dist_bikeSport;
    }else{
      adjList_dist = &adjList_dist_jogger;
    }


    // Rcpp::Rcout<<"agentNo: "<<agentNo<<std::endl;

    //initialize a priority queue
    std::priority_queue<pqPair, std::vector<pqPair>, std::greater<pqPair>> pq;

    //refresh variables for each agent
    int src = src_v[agentNo];
    // int goal = goal_v[agentNo];
    std::vector<double> dist(dist_o);
    //copy AOIList_o into new AOIList. (they're emptied for every agent)
    std::vector<std::string> AOIList(AOIList_o.size());
    std::copy( AOIList_o.begin(), AOIList_o.end(), AOIList.begin() ) ;

    //start with first node (src)
    // Distance of source vertex from itself is always 0
    pq.push(std::make_pair(0.0, src));
    dist[src] = 0.0;

    //prepare vector for agent's information (for each AOI type)
    std::vector<double> aoiDist_v;
    std::vector<int> aoiNode_v;
    std::vector<std::string> aoiType_v;

    //std::vector<double> aoiProb_v;


    // Find shortest path for all vertices
    while(!pq.empty()) {

      // Rcpp::Rcout<<"pq.size()"<< pq.size()<<std::endl;

      //get vertex with shortest total distance from pq (sptv)
      int sptv = pq.top().second;
      pq.pop();

      //IF AN AOI IS REACHED
      std::string AOI = AOI_v[sptv];
      //determine if AOI is in AOIList (not simple in C++)
      auto ptr = std::find(AOIList.begin(), AOIList.end(), AOI);
      //if pointer doesn't point to the end, then AOI was present in AOIList
      if( ptr != AOIList.end() ){

        // Rcpp::Rcout<<"AOI reached!!"<<std::endl;

        //record the AOI type found
        aoiType_v.push_back(AOI);
        //record the associated node
        aoiNode_v.push_back(sptv);
        //record the distance to the associated node
        aoiDist_v.push_back(dist[sptv]);

        //remove AOI type from AOIList
        AOIList.erase(ptr);

        // Rcpp::Rcout<<"AOIList: "<<AOIList<<std::endl;
        // determine if AOIList is now empty
        if(AOIList.empty()){
          // Rcpp::Rcout<<"AOIList is now empty!"<<std::endl;

          //if so, add results to final output list and exit loops
          outputDist[agentNo] = aoiDist_v;
          outputNode[agentNo] = aoiNode_v;
          outputAOI[agentNo] = aoiType_v;

          //break out of two loops to start new agentloop
          goto endAgentLoop;

        }
      }

      //determine relevant adjacency vertices and distances (const refs — no copy)
      const std::vector<int>&    adjVrts = adjList_IDs[sptv];
      const std::vector<double>& adjDist = (*adjList_dist)[sptv];


      //CYCLE ALL ADJACENT VERTICES
      //if needed, update their total distances (dist) and prior vertex
      for(int vrt = 0; vrt < adjVrts.size(); vrt++){
        int adjv = adjVrts[vrt];

        //determine distance between adjacent vertices
        double adjd = adjDist[vrt];


        //if distance is smaller to prior vertex (sptv) with distance between sptv and this vertex (adjv)
        if(dist[adjv] > dist[sptv] + adjd){
          dist[adjv] = dist[sptv] + adjd;

          // Rcpp::Rcout<<"adjd: "<<adjd<<std::endl;
          // Rcpp::Rcout<<"dist[sptv]: "<<dist[sptv]<<std::endl;
          // Rcpp::Rcout<<"dist[adjv]: "<<dist[adjv]<<std::endl;
          // Rcpp::Rcout<<"**********"<<std::endl;


          //record vertex that had shortest path to adjv
          // pathSteps[adjv] = sptv;
          //return pair into pq
          pq.push(std::make_pair(dist[adjv], adjv));

        }

      }

    }


    endAgentLoop:
      ;
    //end of loop for each agent



    double agentSpeed = agentSpeeds[agentNo];
    double agentDuration = agentDurations[agentNo];

    //get maximum/minimum values to standardise later
    double dulnMax = *max_element(AOI_dulns.begin(), AOI_dulns.end());
    double dulnMin = *min_element(AOI_dulns.begin(), AOI_dulns.end());
    double agentDurationMax = *max_element(agentDurations.begin(), agentDurations.end());
    double agentDurationMin= *min_element(agentDurations.begin(), agentDurations.end());

    double areaMax = 15000;//*max_element(AOI_areas.begin(), AOI_areas.end())
    double areaMin = 316;//sqrt(100000m^2)

    double timeDistMax = *max_element(aoiDist_v.begin(), aoiDist_v.end());
    double timeDistMin = *min_element(aoiDist_v.begin(), aoiDist_v.end());

    timeDistMax = timeDistMax/ ((2*1000)); // 2 = slowest agent speed -> maximum timeDist
    timeDistMin = timeDistMin/ ((20*1000)); // 20 = fastest agent
    //
//     Rcpp::Rcout<<"dulnMax: "<<dulnMax<<std::endl;
//     Rcpp::Rcout<<"dulnMin: "<<dulnMin<<std::endl;
//     Rcpp::Rcout<<"agentDurationMax: "<<agentDurationMax<<std::endl;
//     Rcpp::Rcout<<"timeDistMax: "<<timeDistMax<<std::endl;

    // translate distances into times (timeDistances) using agent's speed characteristic (time = distance/speed)
    // and give timeDistances that are bigger than 1/4 of duration a prob value of 0
    //    create a vector as large as aoiDist_v
    std::vector<double> aoiProb_v = std::vector<double>(aoiDist_v.size());

    // Rcpp::Rcout<<"*" <<std::endl;
    // Rcpp::Rcout<<"*" <<std::endl;
    // Rcpp::Rcout<<"*" <<std::endl;
    // Rcpp::Rcout<<"*" <<std::endl;
    // Rcpp::Rcout<<"*" <<std::endl;
    // Rcpp::Rcout<<"*" <<std::endl;
    // Rcpp::Rcout<<"*" <<std::endl;
    // Rcpp::Rcout<<"*" <<std::endl;
    // Rcpp::Rcout<<"*" <<std::endl;
    //
    // Rcpp::Rcout<<"aoiDist_v.size() = "<<aoiDist_v.size() <<std::endl;


    //    cycle through values in aoiDist_v
    for(int probNo = 0; probNo < aoiDist_v.size(); probNo++ ){

      // Rcpp::Rcout<<"agentNo: "<< agentNo <<std::endl;
      // Rcpp::Rcout<<"probNo: "<< probNo <<std::endl;


    //    translate to timeDist using current agent speed



      double timeDist = aoiDist_v[probNo] / ((agentSpeed*1000)); //speed is in km/h, have timeDist in minutes (as durations)

          // Rcpp::Rcout<<"timeDist: "<< timeDist <<std::endl;
          // Rcpp::Rcout<<"agentDuration: "<< agentDuration <<std::endl;

    //set minimum of 0.05 hours = 3min (no diff. between areas 1, 2, or 3 mins away)
          if(timeDist < (0.01)){
            timeDist = (0.01);
            }

      // Rcpp::Rcout<<"aoiDist_v[probNo] = "<<aoiDist_v[probNo] <<std::endl;
      // Rcpp::Rcout<<"agentSpeed = "<<agentSpeed <<std::endl;
      // Rcpp::Rcout<<"agentDuration = "<<agentDuration <<std::endl;
      //
      // Rcpp::Rcout<<"timeDist = "<<timeDist <<std::endl;

      // Rcpp::Rcout<<"timeDist: "<<timeDist<<std::endl;
      //     Rcpp::Rcout<<"agentDuration/60: "<<agentDuration/60 <<std::endl;
      //     Rcpp::Rcout<<"****"<<std::endl;
      // Rcpp::Rcout<<"timeDist: "<<timeDist<<"----"<<std::endl;
      //     Rcpp::Rcout<<"agentDuration: "<<agentDuration<<"----"<<std::endl;

    //    if timeDistance is bigger than 1/4 of agent duration, assign a value of 0 in aoiProb_v
          if(timeDist > (0.1 * (agentDuration/60))){ //translate duration to hours (as speed is based on hours)
            aoiProb_v[probNo] = 0;

            // Rcpp::Rcout<<"aoiProb_v = 0"<<std::endl;

            }else{
            //otherwise assign a prob value of ... )
            // AOI_duln is a List format, works with subsetting
            //double duln = AOI_duln["DULN"][AOI_duln["AOI"] == aoiType_v[probNo]];

            if(AOI_aois.size() > 1){
              //cycle instead through AOI_duln to get duln value
              double duln = 0;
              double dulnSize = AOI_dulns.size();

              double area = 0;
              // Rcpp::Rcout<<"AOI_duln size: "<<dulnSize<<std::endl;


              for(int i = 0; i < dulnSize; i++){
                std::string aoi = AOI_aois[i];

                // Rcpp::Rcout<<"aoi: "<<aoi<<std::endl;

                if(aoi == aoiType_v[probNo]){
                  //assign duln based on AOI
                  duln = AOI_dulns[i];
                  //assign area based on AOI
                  area = sqrt(AOI_areas[i]);
                  if(area > 7000){
                    area = 7000;
                    }
                  // Rcpp::Rcout<<"duln: "<<duln<<std::endl;


                  // break;
                }


            }

            //determine attractivity/distance ratio (~5min to 1 duln)
            // divide by 100 to bring duln to ~1

            //convert values to 0-1
            //duln (max = , min = )
            //added + 0.1 to avoid 0. To keep influence of other factors.
            //Otherwise, 0 * anything is still 0
            // duln = (duln-dulnMin+0.1) / (dulnMax-dulnMin+0.1);
            //
            // agentDuration = agentDuration/agentDurationMax;
            // timeDist = timeDist/timeDistMax;

            // calculate probability for each aoi:
            //standardised duln * standardised duration * (1-standardised distance): inverse of distance as chances get lower as distance gets higher (+0.01 to avoid 0)

            // aoiProb_v[probNo] =  ( ( ((duln-dulnMin+0.1) / (dulnMax-dulnMin+0.1))*5 ) *(agentDuration/agentDurationMax) ) + ((1-(timeDist/timeDistMax)+0.001) * ((1-(agentDuration/agentDurationMax))+0.001 ) ); //

            //use RUM model + MNL (multinomial logit)
            //exp(A*dist + B*utility) / sum(exp(A*dist_i + B*utility_i))
//
//               double dulnStd = ((2-1)*(duln-dulnMin+0.01) / (dulnMax-dulnMin+0.01)) + 1;
//               double timeDistStd = ((2-1)*(timeDist-timeDistMin+0.01)/(timeDistMax-timeDistMin+0.01) ) + 1 ;
//               double agentDurationStd = ((2-1)*(agentDuration-agentDurationMin+0.01)/(agentDurationMax - agentDurationMin+0.01)) + 1;
//               double areaStd = ((2-1)*( area-areaMin )/(areaMax-areaMin+0.01) ) + 1;

              double distPerDuration = timeDist/agentDuration;
              double dulnByAreaPerDuration = duln*area/agentDuration;
              double dulnByArea = duln*area;
              double dulnByDuration = duln*agentDuration;
              double dulnSquByDuration = duln*duln*agentDuration;

              // double distPerDurationMax = 0;
              // double dulnByAreaPerDurationMax = 0;
              // double distPerDurationMin = 10000000;
              // double dulnByAreaPerDurationMin = 10000000;

              // //standardise
              // //get max
              // for(int i = 1; i < src_v.size(); i++){
              //   //do calculations for all agents
              //   //TODO: render more efficient by splitting overall agent loop into two instead
              //
              //   //if distance below 10m, make it 10m (avoids very small numbers or 0s)
              //   double dist = 0;
              //   if(aoiDist_v[i] < 10){
              //     dist = 10;
              //   }else{
              //     dist = aoiDist_v[i];
              //   }
              //
              //   Rcpp::Rcout<<"agentDurations[i]: "<<agentDurations[i]<<std::endl;
              //   Rcpp::Rcout<<"agentSpeeds[i]: "<<agentSpeeds[i]<<std::endl;
              //   Rcpp::Rcout<<"AOI_dulns[probNo]: "<<AOI_dulns[probNo]<<std::endl;
              //   Rcpp::Rcout<<"AOI_areas[probno]: "<<AOI_areas[probNo]<<std::endl;
              //   Rcpp::Rcout<<"******* step1: "<<std::endl;
              //
              //   double distPerDurationMinMax_pot = (dist / (((agentSpeeds[i]*1000)/60)/agentDurations[i]));
              //   double dulnByAreaPerDurationMinMax_pot = AOI_dulns[i]*sqrt(AOI_areas[i])/agentDurations[i];
              //
              //
              //
              //
              //
              //   if(distPerDurationMinMax_pot > distPerDurationMax){
              //     distPerDurationMax = distPerDurationMinMax_pot;
              //   }
              //   if(dulnByAreaPerDurationMinMax_pot > dulnByAreaPerDurationMax){
              //     dulnByAreaPerDurationMax = dulnByAreaPerDurationMinMax_pot;
              //   }
              //   if(distPerDurationMinMax_pot < distPerDurationMin){
              //     distPerDurationMin = distPerDurationMinMax_pot;
              //   }
              //   if(dulnByAreaPerDurationMinMax_pot < dulnByAreaPerDurationMin){
              //     dulnByAreaPerDurationMin = dulnByAreaPerDurationMinMax_pot;
              //   }
              //
              //   Rcpp::Rcout<<"distPerDurationMax: "<<distPerDurationMax<<std::endl;
              //   Rcpp::Rcout<<"dulnByAreaPerDurationMax: "<<dulnByAreaPerDurationMax<<std::endl;
              //   Rcpp::Rcout<<"distPerDurationMin: "<<distPerDurationMin<<std::endl;
              //   Rcpp::Rcout<<"dulnByAreaPerDurationMin: "<<dulnByAreaPerDurationMin<<std::endl;
              //   Rcpp::Rcout<<"******* step2: "<<std::endl;
              //
              //
              //
              // }

              // standardise to from-to : ((to-from)*(x-minX)/(maxX-minX)) + from
              // (1-10)

              //get max and min (theoretical max in min, using max and min of components ex: duln, duratinos etc.)
              double distPerDurationMax = timeDistMax / agentDurationMin ;
              double distPerDurationMin = timeDistMin / agentDurationMax ;
              double dulnByAreaPerDurationMax = (dulnMax*areaMax)/agentDurationMin ;
              double dulnByAreaPerDurationMin = (dulnMin*areaMin)/agentDurationMax;
              double dulnByAreaMax = (dulnMax*areaMax);
              double dulnByAreaMin = (dulnMin*areaMin);

              double dulnByDurationMax = (dulnMax*agentDurationMax);
              double dulnByDurationMin = (dulnMin*agentDurationMin);

              double dulnSquByDurationMax = (dulnMax*dulnMax*agentDurationMax);
              double dulnSquByDurationMin = (dulnMin*dulnMin*agentDurationMin);

              double dulnByAreaPerDurationSTD =  ( (10-1)*(dulnByAreaPerDuration-dulnByAreaPerDurationMin)/(dulnByAreaPerDurationMax-dulnByAreaPerDurationMin) ) + 1;
              double distPerDurationSTD =  ( (10-1)*(distPerDuration-distPerDurationMin)/(distPerDurationMax-distPerDurationMin) ) + 1;

              double dulnByAreaSTD =  ( (10-1)*(dulnByArea-dulnByAreaMin)/(dulnByAreaMax-dulnByAreaMin) ) + 1;
              double dulnByDurationSTD =  ( (10-1)*(dulnByDuration-dulnByDurationMin)/(dulnByDurationMax-dulnByDurationMin) ) + 1;
              double dulnSquByDurationSTD =  ( (10-1)*(dulnSquByDuration-dulnSquByDurationMin)/(dulnSquByDurationMax-dulnSquByDurationMin) ) + 1;


              // Rcpp::Rcout<<"dulnStd: "<<dulnStd<<std::endl;
              // Rcpp::Rcout<<"timeDistStd: "<<timeDistStd<<std::endl;
              // Rcpp::Rcout<<"agentDurationStd: "<<agentDurationStd<<std::endl;
              // Rcpp::Rcout<<"agentDurationStd: "<<agentDurationStd<<std::endl;



            // old formula (linear combination of benefit and cost)
            // aoiProb_v[probNo] = ( 5*( ((( (((duln-dulnMin+0.01) / (dulnMax-dulnMin+0.01))) ) *  sqrt(agentDurationStd) ) ) ) + (1*( ((1- (timeDist-timeDistMin+0.01))/(timeDistMax-timeDistMin+0.01) ))*((1- ( timeDist-timeDistMin+0.01))/(timeDistMax-timeDistMin+0.01) ) * (((1-sqrt(agentDurationStd)) ) ) ) ); //time weighted by time available  //*(1-(agentDuration/agentDurationMax))

              //standardise to 1:10


              //formula follows standard RUM and utility maximization

              // Calibrated formula
              // aoiProb_v[probNo] =  (8.6330*dulnStd * (areaStd/agentDurationStd)) +  (-6.4311 * timeDistStd ) ; //ratio of time by time available  //*(1-(agentDuration/agentDurationMax))


              //new calibrated formula (highest significance in calibration and standardised to 1-10)
              aoiProb_v[probNo] =  (3.23695 * dulnByAreaPerDurationSTD) +  (-6.65703 * distPerDurationSTD ) ; //ratio of time by time available  //*(1-(agentDuration/agentDurationMax))

              // aoiProb_v[probNo] =  -1*distPerDurationSTD  ; //ratio of time by time available  //*(1-(agentDuration/agentDurationMax))


              // if we focus only on distance
              // aoiProb_v[probNo] =  -timeDistStd  ; //ratio of time by time available  //*(1-(agentDuration/agentDurationMax))


              // aoiProb_v[probNo] =   Y*(2-timeDistStd+1) ; //ratio of time by time available  //*(1-(agentDuration/agentDurationMax))

              // aoiProb_v[probNo] =  (X*dulnStd * (areaStd/(2-agentDurationStd+1)) ); //ratio of time by time available  //*(1-(agentDuration/agentDurationMax))

              // aoiProb_v[probNo] =  (X*dulnStd) + (0.5*areaStd*agentDurationStd) - (Z*timeDistStd/agentDurationStd); //ratio of time by time available  //*(1-(agentDuration/agentDurationMax))

              //to do : verify with area
              // Rcpp::Rcout<<"aoiProb_v[probNo]: "<<aoiProb_v[probNo]<<std::endl;

              // Area creating problems with interconnected aois
              // * ((sqrt(area)/sqrt(areaMax)))

              // aoiProb_v[probNo] = ( 0*( ((( ((duln-dulnMin+0.01) / (dulnMax-dulnMin+0.01))) ) * (agentDuration/agentDurationMax) * ((sqrt(area)/sqrt(areaMax)))) ) ) + (1*( (1-(timeDist/timeDistMax )) ) * ((1-(agentDuration/agentDurationMax))) ) ; //time weighted by time available  //*(1-(agentDuration/agentDurationMax))

              // Rcpp::Rcout<<"aoiProb_v[probNo]: "<<aoiProb_v[probNo]<<std::endl;
              //
              // Rcpp::Rcout<<"ATTR: "<<( 1*( ((( ((duln-dulnMin+0.01) / (dulnMax-dulnMin+0.01))) ) * (agentDuration/agentDurationMax) * ((sqrt(area)/sqrt(areaMax)))) ) )<<std::endl;
              // Rcpp::Rcout<<"PROX: "<<(1*( (1-(timeDist/timeDistMax )) ) * ((1-(agentDuration/agentDurationMax))) )<<std::endl;

            //result is called aoiProb, but true probability comes after, when exp(aoiProb)/exp(sum(allProbs))



            // aoiProb_v[probNo] = (((duln-dulnMin+0.1) / (dulnMax-dulnMin+0.1))  *(agentDuration/agentDurationMax)  ) + (2*(1-(timeDist/timeDistMax)+0.001) * ((duln-dulnMin+0.1) / (dulnMax-dulnMin+0.1)) * (1-(agentDuration/agentDurationMax)+0.001));
//+ (area/areaMax)

//
// Rcpp::Rcout<<"duln: "<< duln <<std::endl;
// Rcpp::Rcout<<"timeDist: "<< timeDist <<std::endl;
// Rcpp::Rcout<<"agentDuration: "<< agentDuration <<std::endl;
//
// Rcpp::Rcout<<"** "<<std::endl;
//
// Rcpp::Rcout<<"dulnSTD: "<< ((duln-dulnMin+0.1) / (dulnMax-dulnMin+0.1)) <<std::endl;
// Rcpp::Rcout<<"timeDistSTD: "<< timeDist/timeDistMax <<std::endl;
// Rcpp::Rcout<<"agentDurationSTD: "<< agentDuration/agentDurationMax <<std::endl;
//
// Rcpp::Rcout<<"aoiProb_v: "<< aoiProb_v[probNo] <<std::endl;
// Rcpp::Rcout<<"********* "<<std::endl;
            }else{
            // otherwise assign 1 to probs
              aoiProb_v[probNo] = 1;
            }

          }
    //    more time you have, the higher chances you visit a far away area


    }

    //get sum of all exp(aoiProb_v)
    //copy aoiProv_v vector
    //std::vector<double> exp_aoiProb_v;
    std::vector<double> exp_aoiProb_v = std::vector<double>(aoiProb_v.size());

    // Rcpp::Rcout<<"aoiProb_v.size(): "<< aoiProb_v.size() <<std::endl;

    //loop to get sum
    double sum_exp_aoiProb = 0;
    double alpha = 1;
    for(int k = 0; k < aoiProb_v.size(); k++){
      if(aoiProb_v[k] != 0){
        sum_exp_aoiProb += exp(aoiProb_v[k]/alpha);
      }
    }

    // Rcpp::Rcout<<"sum_exp_aoiProb: "<< sum_exp_aoiProb <<std::endl;
    //
    // Rcpp::Rcout<<"exp_aoiProb_v.size(): "<< exp_aoiProb_v.size() <<std::endl;
    // Rcpp::Rcout<<"*****"<<std::endl;
    // Rcpp::Rcout<<"*****"<<std::endl;
    // Rcpp::Rcout<<"*****"<<std::endl;
    // Rcpp::Rcout<<"*****"<<std::endl;


    std::vector<double> aoiProb_v_result = std::vector<double>(aoiProb_v.size());
    //loop to get individual values (exp(x)/sum(exp(all x)))
    for(int j = 0; j < aoiProb_v.size(); j++){

      //if zero, make it zero probability
      if(aoiProb_v[j] == 0){
        exp_aoiProb_v[j] = 0;
      }else{
        //get individual exp()
        exp_aoiProb_v[j] = exp(aoiProb_v[j]/alpha);
      }



      // Rcpp::Rcout<<"aoiProb_v[j]] normal: "<< aoiProb_v[j] <<std::endl;
      // Rcpp::Rcout<<"exp_aoiProb_v[j]: "<< exp_aoiProb_v[j] <<std::endl;

      //divide by sum of exp
      if(sum_exp_aoiProb != 0){
        aoiProb_v_result[j] = exp_aoiProb_v[j] / sum_exp_aoiProb;
      }else{
        aoiProb_v_result[j] = 0;
      }

      // Rcpp::Rcout<<"aoiProb_v[j]: "<<aoiProb_v_result[j]<<std::endl;


      // if(aoiProb_v[j] == 0){
      //   Rcpp::Rcout<<"sum_exp_aoiProb: "<< sum_exp_aoiProb <<std::endl;
      //
      //   Rcpp::Rcout<<"aoiProb_v[j]] probability: "<< aoiProb_v[j] <<std::endl;
      //   Rcpp::Rcout<<"***********************"<<std::endl;
      //   Rcpp::Rcout<<"***********************"<<std::endl;
      // }





    }



    //populate output vector with aoiProb_v for this agent
    outputProb[agentNo] = aoiProb_v_result;


  }

  log_file.close();

  return List::create(Named("distances") = wrap(outputDist), Named("nodes") = wrap(outputNode), Named("aoi") = wrap(outputAOI), Named("prob") = wrap(outputProb) );
}



/*
 * determineChoiceProbabilities:
 * takes list of current vertices (every agent making decision), DataFrames edgeTable and vertexTable and pointer to adjacency list.
 * returns list of probabilities for every path choice for every agent
 */

// // [[Rcpp::export]]
// List determineChoiceProbabilities_cpp(IntegerVector currentVs,
//                                       DataFrame edgeTable,
//                                       DataFrame vertexTable,
//                                       Rcpp::XPtr< std::vector<std::vector<int>> > adjList_IDs
//                                       ){
//
//
//
//
// }
