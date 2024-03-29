pmw <- function(seqaa, Ar = c(C = 12.0107, H = 1.00794, O = 15.9994,
                              N = 14.0067, P = 30.973762, S = 32.065), gravity = 9.81,
                unit = "gram", checkseqaa = TRUE){
    #
    # Check arguments:
    #
    if( ! unit %in% c("gram", "N") ) stop("Non allowed unit argument") 
    if( any(Ar < 0)) stop("Negative Ar not allowed")
    if( gravity < 0 ) stop("Antigravity not allowed")
    if( nchar(seqaa[1]) > 1) stop("seqaa should be a vector of single chars")
    allowed <- s2c("*ACDEFGHIKLMNPQRSTVWY")
    if(checkseqaa) if(! all(seqaa %in% allowed) ) warning("Non allowed characters in seqaa")
    #
    # Compute aa frequencies:
    #
    comp <- table(factor(seqaa, levels = allowed))
    if( sum(comp[-which(names(comp) == "*")]) == 0) stop("Zero length protein not allowed")
    #
    # Compute the molecular weigth of one mol of an amino-acid from its composition:
    #
    aamw <- function(chonps){
        mw <- chonps[1]*Ar["C"] + chonps[2]*Ar["H"] + chonps[3]*Ar["O"] +
            chonps[4]*Ar["N"] + chonps[5]*Ar["P"] + chonps[6]*Ar["S"]
        if(unit == "gram"){
            return(mw*gravity/9.81)
        } else {
            return(mw*gravity/1000)
        }
    }
    
    #
    # Compute molecular weight:
    #
    mw <- comp["A"]*aamw(c(3,7,2,1,0,0)) +
        comp["C"]*aamw(c(3,7,2,1,0,1)) +
        comp["D"]*aamw(c(4,7,4,1,0,0)) +
        comp["E"]*aamw(c(5,9,4,1,0,0)) +
        comp["F"]*aamw(c(9,11,2,1,0,0)) +
        comp["G"]*aamw(c(2,5,2,1,0,0)) +
        comp["H"]*aamw(c(6,9,2,3,0,0)) +
        comp["I"]*aamw(c(6,13,2,1,0,0)) +
        comp["K"]*aamw(c(6,14,2,2,0,0)) +
        comp["L"]*aamw(c(6,13,2,1,0,0)) +
        comp["M"]*aamw(c(5,11,2,1,0,1)) +
        comp["N"]*aamw(c(4,8,3,2,0,0)) +
        comp["P"]*aamw(c(5,9,2,1,0,0)) +
        comp["Q"]*aamw(c(5,10,3,2,0,0)) +
        comp["R"]*aamw(c(6,14,2,4,0,0)) +
        comp["S"]*aamw(c(3,7,3,1,0,0)) +
        comp["T"]*aamw(c(4,9,3,1,0,0)) +
        comp["V"]*aamw(c(5,11,2,1,0,0)) +
        comp["W"]*aamw(c(11,12,2,2,0,0)) +
        comp["Y"]*aamw(c(9,11,3,1,0,0))
    #
    # Remove n - 1 water molecules:
    #
    mw <- mw - (sum(comp[-which(names(comp) == "*")]) - 1)*aamw(c(0,2,1,0,0,0))
    names(mw) <- NULL
    return(mw)
}