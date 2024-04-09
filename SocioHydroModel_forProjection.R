#This is the sociohydro model for the model projections

SocHydModel_SurgePrecipHousing <- function(
        result_2021,   #this extracts the result of sh model for 2021 from the model run through calibration period
        Year,          #years to run simulation through
        W,             #annual maxima discharge
        Precip,        #annual maxima rainfall
        Surgethreshold = 2,      #flooding threshold (storm surge)
        Rainthreshold = 0.1, #flooding threshold (precipitation)
        alphad = 1,    #risk taking attitude
        alphaa = 15,   #anxiousness
        alphap = 1,    #activeness
        alphar = 0.25, #effectiveness of preparedness
        Br = 1,        #flood to loss
        mewa = 0.10,   #forgetfulness
        mewp = 0.04,   #decay rate of precautionary measures
        Dmax = 1.2,    #maximum settlement density
        Pmax = 1.0,    #maximum preparedness
        Rmax = 1.0,    #maximum relative loss
        Amax = 1.0,    #maximum awareness
        U_rate = 0.5,  #growth rate
        POT_S_max = 3, #3.5 m
        POT_R_max = 0.15, #0.15 m
        
        #housing
        b1 = 0.5,
        b2 = 0.5, 
        b3 = 0.05,
        duration = 5,
        HPmax = 1.2
){
    #Initialize vectors
    POT = rep(0,length(W))          #Peak over Threshold
    POT_S = max(W) - Surgethreshold
    POT_R = max(Precip) - Rainthreshold
    L = rep(0,length(W))            #L - loss
    D = rep(0.1*Dmax,length(W))     #D - population density
    R = rep(0,length(W))            #R - relative loss
    U = rep(U_rate,length(W))       #U - growth rate of settlement density
    A = rep(0.3 * Amax,length(W))     #A - awareness
    P = rep(0.3 * Pmax,length(W))     #P - preparedness
    dDdt = rep(0,length(W))
    dPdt = rep(0,length(W))
    dAdt = rep(0,length(W))
    HP = rep(0.25 * HPmax,length(W)) #what should be our initial HP values ?
    tflood = rep(50,length(W))
    
    #Initialize values for the start of simulations
    L[1] = result_2021$L[1] #remember to load the result_2021 file in the Rscript that runs simulation
    D[1] = result_2021$D[1]
    A[1] = result_2021$A[1]
    HP[1] = result_2021$HP[1]
    
    #Find peak over threshold for surge and rain
    for (t in 1:length(W))
    { 
        POT_S[t] = max(0,1*W[t] - Surgethreshold)
        POT_R[t] = max(0,1*Precip[t] - Rainthreshold)
    }
    
    #scale peak over threshold
    POT_S_Scaled = POT_S/POT_S_max
    POT_R_Scaled = POT_R/POT_R_max
    
    #Find the maximum peak over threshold for each timestep
    for (t in 1:length(W))
    {
        POT[t] = max(POT_S_Scaled[t],POT_R_Scaled[t])
    }
    
    #sociohydro loop, all the variables are calculated based on sociohydro models differential equations
    for (t in 2:length(W))
    {
        #Relative loss
        if( POT[t] > 0)
        {R[t] = max(0,Rmax - Br*exp(-alphar*(Pmax - P[t-1])*POT[t]))}
        else
        {R[t] = 0}
        
        #Loss
        L[t] = R[t] * D[t-1] * HP[t-1] 
        
        #Density
        dDdt[t] = U[t]*(1 - alphad*A[t-1])*D[t-1]*(1 - D[t-1]/Dmax)
        
        #Awareness
        dAdt[t] = alphaa*L[t]*(1 - A[t-1]/Amax) - mewa*A[t-1]
        
        #Preparedness
        if (R[t] > 0)
        {dPdt[t] = alphap*dAdt[t]*(1 - P[t-1]/Pmax) - mewp*P[t-1]
        tflood[t] = 0}
        else
        {dPdt[t] = -1*mewp*P[t-1]
        tflood[t] = tflood[t-1] + 1}
        
        #housing
        if (R[t] == 0 && tflood[t] >= duration)
        {
            HP[t] = HP[t-1] + b1*(HPmax - HP[t-1])
            tflood[t] = tflood[t-1] + 1
        }
        if (R[t] == 0 && tflood[t] < duration)
        {
            HP[t] = HP[t-1] - b3*(HPmax - HP[t-1])
            tflood[t] = tflood[t-1] + 1
        }
        if (R[t] > 0)
        {
            
            HP[t] = HP[t-1] - b2*L[t]*HP[t-1] 
            tflood[t] = 0 #restarts counter from last loss event
        }
        
        #Compute D A P and HP for each timestep
        D[t] = max(0, min(D[t-1] + dDdt[t],Dmax))
        A[t] = max(0, min(A[t-1] + dAdt[t],Amax))
        P[t] = max(0, min(P[t-1] + dPdt[t],Pmax))
        HP[t] = max(0, min(HP[t],HPmax)) 
        
    }
    
    #round the output to only three decimal places for memory optimization
    W = round(W,3)
    Precip = round(Precip,3)
    L = round(L,3)
    D = round(D,3)
    R = round(R,3)
    A = round(A,3)
    P = round(P,3)
    HP = round(HP,3)
    
    #SocHydResults<-data.frame(Year , W, Precip, L, D, R, U, A, P, HP)
    SocHydResults<-data.frame(Year, L, D, A, HP) #Just keeping variables of interest
    return(SocHydResults)
}

 