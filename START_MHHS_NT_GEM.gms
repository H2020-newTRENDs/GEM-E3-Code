$ONTEXT
LES Demand
$OFFTEXT

$setglobal path "C:\PROJECTS\MHHS\"
Set
i                        Products                /agr,  ind, ser/
id                       Income quantiles        /d1*d5/
io                       Input Output Categories /QQ,HC,LAV,CAP/
n                        Iterations               /1*30/
;

Alias (i,j),(id,idd);

Parameter
* Data
Data(io,i)               Data
dataHC(id,i)             Household Consumption per quantile
dataCI(id,i)             Capital income
dataLI(id,i)             Labour income
dataFrisch(id)           Frisch Parameter per income quantile

* Production
qbar(i)                  Base year production
kbar(i)                  Base year capital
lbar(i)                  Base year labour
pbar(i)                  Base year unit production cost
pkbar                    Base year unit cost of capital
plbar                    Base year unit cost of labour
thetaK(i)                Capital Value Share
thetaL(i)                Labour Value Share
sigma                    Elasticity of Substitution
rho                      Substitution parameter
thetaKid(id,i)           Ownerhsip share of firms from Households
thetaLid(id,i)           Ownerhsip share of Labour from Households
ks                       Capital Stock
ls                       Labour Stock
* Consumption
hcbar                    Base year consumption
abs(i)                   Average budget share
beta(i)                  Consumption pattern
mmbar                    Base year Income
hcbar                    Base year household consumption
IE(i)                    Income Elasticity
frisch                   Frisch parameter
minsub(i)                Subsistence minima
bu_abs(id,i)             Average budget share
bu_beta(id,i)            Consumption pattern
bu_mmbar(id)             Base year Income
bu_hcbar(id,i)           Base year household consumption
bu_IE(id,i)              Income Elasticity
bu_frisch(id)            Frisch parameter
bu_MM(id)                Income
bu_HC(id,i)              Household consumption per income class
bu_minsub(id,i)          Subsistence minima - Income category
CompareEV(*,id)          Compare Equivalent Variation

* Scenario
tx(i)                    Consumer Tax
alloc(id)                Allocate Consumer Tax to Household Income

* Reporting
Report(*,*,i)
UTILITY_ref              Utility in Refernce
UTILITY_scen             Utility in Scenario
INCOME_ref               Income in Reference
IU(id)                   Indirect utility
MMIU(id)                 Money metric of indirect utility
EV_satellite(id)         Equivalent Variation [Single Step case]
EV_satellite2(n,id)      Equivalent Variation [Loopcase]
EV_endo(id)              Equivalent Variation [endogenous multi household]
CV(id)                   Compensating Variation
PHC_ref(i)               Reference Consumer Price
bMM_ref(id)              Reference Income of Multiple Households

* Checks
chk_zeroporfit(i)        Zero Profit
chk_incomeexhs           Income
chk_commodbaln(i)        Market clearing
CheckIncome              Check that Benchmark SH Income equals benchmark sum of MH income
CheckConsumption         Check that Benchmark SH Consumption equals benchmark sum of MH consumption
chkMKT                   Check that Benchmark SH Production equals benchmark sum of MH Consumption
;

Positive Variables
QQ(i)                    Output
KA(i)                    Capital
LA(i)                    Labour
PQ(i)                    Unit Cost of production
PHC                      Price of Household Consumption
PK                       Unit cost of capita
PL                       Unit cost of labour
HC(i)                    Household demand of products
bHC(id,i)                Household demand of products
MM                       Household Income
bMM(id)                  Household Income
RECYCL(id)               Recycling revenues by income class
;

execute "GDXXRW i=%path%Data_MHHS.xlsx o=%path%Data_MHHS.gdx index=INDEX!a1";
execute_load '%path%Data_MHHS.gdx', Data, dataHC, dataCI, dataLI, dataFrisch;

*========================================== Calibration
*------------------------------------------------------
* Prices
PQ.l(i)           = 1;
pbar(i)           = PQ.l(i);
PK.l              = 1;
pkbar             = PK.l;
PL.l              = 1;
plbar             = PL.l;
PHC.l(i)          = PQ.l(i);

* Firms
QQ.l(i)           = Data("QQ",i)/PQ.l(i);
qbar(i)           = QQ.l(i);
KA.l(i)           = Data("CAP",i)/PQ.l(i);
kbar(i)           = KA.l(i);
KS                = sum(i ,kbar(i));
LA.l(i)           = Data("LAV",i)/PQ.l(i);
lbar(i)           = LA.l(i);
LS                = sum(i, lbar(i));
thetaK(i)         = PK.l*KA.l(i)/(PK.l*KA.l(i)+PL.l*LA.l(i));
thetaL(i)         = (1-thetaK(i));
sigma             = 0.5;
rho               = (sigma - 1)/sigma;

* Representative Household
frisch            = -2;
ie(i)             = 1 ;
MM.l              = sum(i,PK.l * KA.l(i)) + sum(i, PL.l * LA.l(i));
mmbar             = MM.l  ;
HC.l(i)           = Data("HC",i)/PQ.l(i);
hcbar(i)          = HC.l(i);
abs(i)            = PQ.l(i)*HC.l(i)/MM.l ;
beta(i)           = abs(i)*ie(i);
minsub(i)         = HC.l(i)* (1 + ie(i)/frisch);

* Household separated by income class
bu_frisch(id)     = -dataFrisch(id);
bu_ie(id,i)       = 1 ;
thetaKid(id,i)    = dataCI(id,i)/sum((idd,j), dataCI(idd,j));
thetaLid(id,i)    = dataLI(id,i)/sum((idd,j), dataLI(idd,j));
bMM.l(id)         = sum(i,thetaKid(id,i) * PK.l*KS) + sum(i, thetaLid(id,i)*PL.l*LS);
bu_HC(id,i)       = dataHC(id,i);
bu_abs(id,i)      = PQ.l(i)*bu_HC(id,i)/bMM.l(id);
bu_beta(id,i)     = bu_abs(id,i) * bu_ie(id,i);
bu_minsub(id,i)   = bu_HC(id,i) * (1 + bu_ie(id,i) /bu_frisch(id)  );
bHC.l(id,i)       = bu_minsub(id,i) + bu_beta(id,i)/PQ.l(i)*(bMM.l(id) - sum(j, PQ.l(j)*bu_minsub(id,j)));

alloc("d1")       =1;

* Calibration Checks
chkMKT(i)              =  (QQ.l(i)  - sum(id, bHC.l(id,i))) * (QQ.l(i)  - sum(id, bHC.l(id,i)));
CheckIncome            =  (sum(id, bMM.l(id)) - MM.l) * (sum(id, bMM.l(id)) - MM.l)    ;
CheckConsumption(i)    =  (sum(id, bHC.l(id,i)) -HC.l(i)) *  (sum(id, bHC.l(id,i)) -HC.l(i)) ;

Loop(i,
         If((chkMKT(i)  >= 1E-06), abort "error in market clearing";
         else
                 if ((CheckConsumption(i)  >= 1E-06), abort "error in balancing household consumption SHC <> MHC";
         else
                         if ((CheckIncome  >= 1E-06), abort "error in balancing household income SHC <> MHC"
                    )
                             )
            )

    );


* Numeraire
PL.fx = 1;

Equations
EQ_QQ(i)                 Unit cost of Production
EQ_KAD(i)                Capital Demand
EQ_LAD(i)                Labour Demand
EQ_MM_RH                 Income - Single Household
EQ_MM_ID(id)             Income - Multi Households
EQ_HC_RH(i)              Private Consumption - Single Household
EQ_HC_MH(id,i)           Private Consumption - Multi Household
EQ_KAS                   Capital Market
EQ_LAS                   Labour Market
EQ_MKT_RH(i)             Commodity Market - Single Household
EQ_MKT_ID(i)             Commodity Market - Multiple Household
EQ_TAX(i)                Consumer Price incl. Tax
EQ_GOVREV(id)            Recycled tax revenues
;

tx(i)=0;
* Simplify writing
$macro CES_PRICE(i) [pbar(i)*[thetaK(i)*(PK/pkbar)**(1-sigma) + thetaL(i)*(PL/plbar)**(1-sigma)]**(1/(1-sigma))]
$macro CES_KA(i)     kbar(i)/qbar(i) * QQ(i) * (PQ(i)*pkbar/(PK*pbar(i)) )**sigma
$macro CES_LA(i)     lbar(i)/qbar(i) * QQ(i) * (PQ(i)*plbar/(PL*pbar(i)) )**sigma
$macro LES_sHC(i)    minsub(i) + beta(i)/PHC(i) *(MM      - sum(j, PHC(j)* minsub(j)))
$macro LES_mHC(id,i) bu_minsub(id,i) + bu_beta(id,i)/PHC(i)*(bMM(id) - sum(j, PHC(j)*bu_minsub(id,j)))

*----- Model

EQ_QQ(i)..       PQ(i)      =e= CES_PRICE(i);

EQ_LAD(i)..      LA(i)      =e= CES_LA(i);

EQ_HC_RH(i)..    HC(i)      =e= LES_sHC(i);

EQ_HC_MH(id,i).. bHC(id,i)  =e= LES_mHC(id,i);

EQ_MM_RH..       MM         =e= PK*KS  + PL*LS + sum(id, RECYCL(id));

EQ_MM_ID(id)..   bMM(id)    =e= sum(i,thetaKid(id,i)*PK*KS) + sum(i, thetaLid(id,i)*PL*LS) + RECYCL(id);

EQ_KAS..         KS         =e= sum(i, CES_KA(i));

EQ_MKT_RH(i)..   QQ(i)      =e= HC(i);

EQ_MKT_ID(i)..   QQ(i)      =e= sum(id,  bHC(id,i)) ;

EQ_TAX(i)..      PHC(i)     =e=  PQ(i)*(1+tx(i)) ;

EQ_GOVREV(id)..  RECYCL(id) =e=  sum(i, tx(i) * QQ(i)) * alloc(id) ;

Model SHH Single Household
/
EQ_QQ.QQ
EQ_LAD.LA
EQ_HC_RH.HC
EQ_MM_RH.MM
EQ_KAS.PK
EQ_MKT_RH.PQ
EQ_TAX.PHC
EQ_GOVREV.RECYCL
/
;

Model MHH Multi Household
/
EQ_QQ.QQ
EQ_LAD.LA
EQ_HC_MH.bHC
EQ_MM_ID.bMM
EQ_KAS.PK
EQ_MKT_ID.PQ
EQ_TAX.PHC
EQ_GOVREV.RECYCL
/

solve SHH using mcp;
* Keep/Store Reference Values
PHC_ref(i) = PHC.l(i);
bMM_ref(id)= sum(i,thetaKid(id,i)*PK.l*KA.l(i)) + sum(i, thetaLid(id,i)*PL.l*LA.l(i)) + RECYCL.l(id);;

* Scenario Increase tax
tx(i)=0.001;
solve SHH using mcp;

* Model Checks  - Single Household
chk_zeroporfit(i)  =  [PQ.l(i) * QQ.l(i)  - PK.l*KA.l(i)  - PL.l*LA.l(i)]   * [PQ.l(i) * QQ.l(i)  - PK.l*KA.l(i)  - PL.l*LA.l(i)];
chk_incomeexhs     =  [sum(j, PHC.l(j)*HC.l(j)) -  MM.l]                    * [sum(j, PHC.l(j)*HC.l(j)) -  MM.l];
chk_commodbaln(i)  =  [ QQ.l(i)  -  HC.l(i)]              * [ QQ.l(i)  -  HC.l(i)];

Loop(i,
         If((chk_zeroporfit(i)  >= 1E-06), abort "error in zeroprofit";
         else
                 if ((chk_incomeexhs   >= 1E-06), abort "error in income exhaustion";
         else
                         if ((chk_commodbaln(i)  >= 1E-06), abort "error in commodity market clearing"
                    )
                             )
            )

    );


*----- Evaluate household demand with new prices and income ----
* One Step evaluation
bu_MM(id)   = sum(i,thetaKid(id,i)*PK.l*KA.l(i)) + sum(i, thetaLid(id,i)*PL.l*LA.l(i)) + RECYCL.l(id);
bu_HC(id,i) = bu_minsub(id,i) + bu_beta(id,i)/PHC.l(i)*(bu_MM(id) - sum(j, PHC.l(j)*bu_minsub(id,j)));

*----- Compute effect on welfare [One Step]----
EV_satellite(id) = prod(i,(PHC_ref(i)/bu_HC(id,i))**bu_beta(id,i)) *(bu_MM(id)  - sum(j, PHC.l(j)*bu_minsub(id,j)))
                                                     - (bMM_ref(id) - sum(j, PHC_ref(j)*bu_minsub(id,j)))
;


* Recalibrate SH model
* Perform n iterations - no convergence criterion included
Loop(n$(ord(n) le card(n)),

         abs(i)            = sum(id, PHC.l(i)*bu_HC(id,i))/sum(id, bu_MM(id))  ;
         beta(i)           = abs(i)*ie(i);
         minsub(i)         = sum(id, PHC.l(i)*bu_HC(id,i))* (1 + ie(i)/frisch);

* Solve SH model with updated parameters
         solve SHH using mcp;

*----- Evaluate household demand with new prices and income [multi step] ----
         EV_satellite2(n,id) = prod(i,(PHC_ref(i)/bu_HC(id,i))**bu_beta(id,i)) *(bu_MM(id)  - sum(j, PHC.l(j)*bu_minsub(id,j)))
                                                     - (bMM_ref(id) - sum(j, PHC_ref(j)*bu_minsub(id,j))) ;
     );


*----- Solve the endogenous multi household case and compare with the satelite module solution
solve MHH using mcp;

EV_endo(id) = prod(i,(PHC_ref(i)/bHC.l(id,i))**bu_beta(id,i)) *(bMM.l(id)  - sum(j, PHC.l(j)*bu_minsub(id,j)))
                                                     - (bMM_ref(id) - sum(j, PHC_ref(j)*bu_minsub(id,j)))
;

CompareEV("Endo",id)             = EV_endo(id);
CompareEV("Exog_One",id)         = EV_satellite(id);
CompareEV("Exog_N_Iter",id)      = EV_satellite2("30",id);