! most information from http://www.servicecanada.gc.ca/eng/services/pensions/cpp/
! specific rules for qc will need to be inputed
module cpp
	implicit none	
	integer ncppyears, cppfirstyear, cpplastyear
	parameter (ncppyears = 165, cppfirstyear = 1966, cpplastyear = 2130)
	integer ncppbyears, cpplastbyear, cppfirstbyear
	parameter (ncppbyears= 162, cpplastbyear = 2065, cppfirstbyear = 1904)
	integer:: era(ncppbyears)		! youngest age can draw benefits
	integer:: nra(ncppbyears)		! full or normal claiming age
	integer:: lra(ncppbyears)		! last age claims
	integer:: nympe(ncppyears)	! number of last years used to compute avg ympe	
	integer:: survagecutoff1(ncppyears)	! Age 1 for which surv pen changes
	integer:: survagecutoff2(ncppyears)	! Age 2 for which surv pen changes
	double precision:: reprate(ncppyears), reprate_s1(ncppyears), transit_rate(ncppyears), &
		reprate_s2(ncppyears) 	! replacement rate of ape
	double precision:: arf(ncppyears)	 	! annual reduction factor (for early retirement)
	double precision:: drc(ncppyears)	 	! annual credit factor (for late retirement)
	double precision:: ympe(ncppyears), ympe_s2(ncppyears)	 	! maximum gainful amount
	double precision:: base(ncppyears)  		! basic exemption for contributions  
	double precision:: workcrate(ncppyears), workcrate_qpp(ncppyears) 	! employee contribution rates
	double precision:: empcrate(ncppyears), empcrate_qpp(ncppyears)	! employer contribution rate
	double precision:: selfcrate(ncppyears), selfcrate_qpp(ncppyears)		! self-employed contribution rate
	double precision:: gendroprate(ncppyears), gendroprate_qpp(ncppyears) 	! General dropout rate
	double precision:: survrate1(ncppyears),survrate2(ncppyears), survrate_s(ncppyears)
	
	!parametres pour rente suvivant 
	double precision:: U1(ncppyears),U3(ncppyears),U4(ncppyears)	! prestations uniformes
	double precision:: maxcombined60(ncppyears), maxcombined65(ncppyears)		! maximum pour la rente combinee	

	integer			:: cppfirstyear_s1, cppfirstyear_s2	! first year for which we have cpp params for the 2 supplements (last year is the same as the basic CPP)
	
   	contains
   	
	!load cpp params from cpp.csv file
	subroutine loadcpp
		integer 			::i, buffer     	
		open(1,file= '../params/cppyear.csv')
		do i=1,ncppyears
			read(1,*)buffer, ympe(i), base(i),workcrate(i),empcrate(i),selfcrate(i),arf(i),drc(i), &
			 nympe(i), reprate(i),gendroprate(i),workcrate_qpp(i),empcrate_qpp(i),selfcrate_qpp(i),gendroprate_qpp(i), &
			 U1(i),U3(i),U4(i), maxcombined60(i), maxcombined65(i), survagecutoff1(i), survagecutoff2(i), &
			 survrate1(i), survrate2(i)
		end do
		close(1)
			
		open(1,file= '../params/cppbyear.csv')
		do i=1,ncppbyears
			read(1,*)buffer, era(i),nra(i),lra(i)
		end do
		close(1)
	end subroutine loadcpp

	! Find Avg (annual) Pensionable Earning with real rules following the steps of:
	! http://retirehappy.ca/how-to-calculate-your-cpp-retirement-pension/   
	double precision function aape(byear,year,age,earnh,nearn,qcdummy)
		
		integer, intent(in)		:: byear 	! birth year
		integer, intent(in)		:: year	! current year 
		integer, intent(in) 	:: age ! age at which cpp pension starts (60-70); should normally be current age
		logical, intent(in)     :: qcdummy !true if qc (use qpp instead of cpp)
		integer, intent(in) 	:: nearn
		double precision, intent(in) :: earnh(nearn)
		logical disabh(16)
		logical kid0_6h(58)

		integer 	:: ycstart, ycstop 	! years contribution period starts and year it end	
		integer 	:: cyears		! number of contributory years
		integer   :: nydrop 		! number of years to drop for gen dropout
		integer   :: nmdrop		! number of remaining months to drop (less than 12)
		integer	:: ngendrop	! number of periods to drop for general dropout
		
		integer	:: i,h, t , bt
		integer	::j(1)		! need this to use minloc function
		
		double precision::upe(100)  ! Unadjusted Pensionable Earning
		double precision::ape(100) ! Adjusted Pensionable Earning
						
		logical :: dropped(100) ! .true. if period is dropped (disab-CRDP or general dropout)
	
		double precision	::tape	 ! Total Adjusted Pensionable Earning 
		double precision  ::avgympe ! Avg Max Pensionable Earning of the nympe last years
		double precision  ::avgape 	 ! Avg  Adjusted Pensionable Earning
		
		double precision:: earntest(58) = 0.0d0
		
		disabh(:) = .false.
		kid0_6h(:) = .false.

		t= year-cppfirstyear+1
		bt= byear-cppfirstbyear+1
			
	
		! Step 1: Calculate nb of contributory year (should be months for more precision)
			
			!Contributory period begins either the month after you turn 18 or in January 1966, whichever is later
			ycstart = max(1966, byear+18) 
			!It ends the month you turn 70 (lra) or the month before your CPP retirement pension starts, whichever is earlier. 
			ycstop = min(byear+lra(bt),byear+age)
			cyears= ycstop-ycstart+1							
			
		! Step 2: Calculate Total Adjusted Pensionable Earning (TAPE)
		
	
			upe=-1.0d0
			ape=-1.0d0
			dropped(:) = .false.
				
			!First calculate Unadjusted Pensionable Earning (UPE) for each contribution year			
			!upe = max(0.0, min( earnh(1:cyears),ympe(t))-base(t))  !upe is the amount between base amount and max pen earn
			! or  it is zero when earning is less than base
			do i=1, cyears, 1
				upe(i) = min(earnh(i),ympe(year - cyears - cppfirstyear+i)) ! earnh(i) assumes contr period starts at 18 
				!write(*,*) "UPE,0,",earnh(i),",",year,",",cyears,",",cppfirstyear,",",i,",",&
				!	ympe(year - cyears - cppfirstyear+i),",",base(year-cyears -cppfirstyear+i),",",upe(i)
					if (upe(i) < base(year-cyears -cppfirstyear+i)) then
					upe(i) = 0.0d0
					end if
					
			end do

			! check for diability benefit periods from 60 to 64 (era to nra)
			do i = era(bt),min(age-1 , nra(bt)-1)
				if(disabh(i-60+1))then	! check if disabled in arrays corresponding to yrs from ealy to before normal ret age 
					upe(i-18+1 + min(0, byear-1948)) = 0.0	! exclude income from upe count 					
					dropped(i-18+1 + min(0, byear-1948)) =  .true.
				end if	
			end do
			
			! Exclude from contr periods
			!cyears=cyears-count(dropped)
			! Find avg ympe in the five (nympe) last contr years ending in the yr cpp benefits start
			avgympe = sum(ympe(ycstop-nympe(t)+1-cppfirstyear+1: ycstop-cppfirstyear+1))/dble(nympe(t))		
						
			! Calculate APE for each year: divide UPE by YMPE for each year and then multiply by avgympe
			do  i = 1, cyears
				ape(i) = upe(i)/ympe((year- cyears+count(dropped) -cppfirstyear+i))*avgympe 
				!write(*,*) byear,age,i, earnh(i), ape(i), upe(i)
			end do

			! TAPE is the sum of all APE 
			tape = sum(ape(1:cyears)) 
			
		! Step 3: Determine dropout periods	
			
			! Child Rearing Drop Out Provision (CRDP) - see http://retirehappy.ca/child-rearing-dropout/
			! Three steps: CRDO1, CRDO2 and general dropout  (disability already exlcuded frome cyears)
							
			! CRDO1: Drop any period where children were under age 7 (if prim. caregiver) and earning was
			! less than basic exemp (UPE=0)	
				do  i = 1, cyears
					if(upe(i) ==0.0d0 .and. kid0_6h(i) ) then ! kidh(i) assumes contr period starts at 18			
						dropped(i) = .true.	
					end if		
				end do	

			!  Find avg APE - exlcuding CRDO1 dropout periods or disability years
				avgape = 	tape/dble(cyears-count(dropped))
					
			! CRDO2: Drop any period where child is under age 7 and APE is less than avg APE	
				do  i = 1, cyears	
					if(ape(i) <avgape .and. kid0_6h(i)) then  ! kidh(i) assumes contr period starts at 18	
						upe(i) =0.0d0
						ape(i) =0.0d0
						dropped(i) = .true.	
					end if		
				end do																	
				
			! General dropout: Drop the X% lowest earning remaining periods.  Everyone's eligible.
			
			if(qcdummy) then

                ! number of complete years (12 months) to drop
                nydrop = floor(gendroprate_qpp(t)*dble(cyears-count(dropped)))

                ! number of remaining months to drop
                 nmdrop = nint((gendroprate_qpp(t)*dble(cyears-count(dropped))-dble(floor(gendroprate_qpp(t)* &
                 & dble(cyears-count(dropped)))))*12)

		    else
				! number of complete years (12 months) to drop	
				nydrop = floor(gendroprate(t)*dble(cyears-count(dropped)))
												
				! number of remaining months to drop 
				 nmdrop = nint((gendroprate(t)*dble(cyears-count(dropped))-dble(floor(gendroprate(t)* &
				 & dble(cyears-count(dropped)))))*12)

            end if

			! After-65-dropout (see Step 6 of http://retirehappy.ca/how-to-calculate-your-cpp-retirement-pension/)
				
				if(age>nra(bt) .and. age < lra(bt)) then	
					
					do i = nra(bt) , age-1  ! age -1 is the last contributory period
						! If you're still working after 65 (and if these earnings are higher than avgAPE) , you can use these earning to replace any period under age 65
						!write(*,*) age, byear , nra(bt)
						if(ape(i-18+1 + min(0, byear-1948)) >= avgape) then 
							nydrop = nydrop + 1 
						
						! If you're not working after 65 or if earnings are less than avgAPE, you can drop periods after 65.
						else
							upe(i-18+1+ min(0, byear-1948)) =0.0
							ape(i-18+1+ min(0, byear-1948)) =0.0
							dropped(i-18+1+ min(0, byear-1948-1)) = .true.	
						end if

					end do
				
				end if	
				
			 ! Find yrs to be dropped (those with lowest income)
			 	do i=1,nydrop
			 		j = minloc(ape, .not. dropped)
			 		upe(j) = 0.0
			 		ape(j) = 0.0
			 		dropped(j) = .true.				
			 	end do 
				 	 
			 	if(nmdrop>0) then	! If we have to find a year to partly exclude 
					j = minloc(ape, .not. dropped)
					upe(j) = (1.0d0-dble(nmdrop)/dble(12)) * upe(j)
			 		ape(j) = (1.0d0-dble(nmdrop)/dble(12)) * ape(j) 	 								
				end if
		
		! Step 4: Calculate Average Annual (should be Monthly) Pensionable Earning (AAPE)	
			
			! Recalculate after-dropout TAPE
			tape = sum(ape(1:cyears)) 

			! AAPE - divide by cyears minus all dropped years (disab, CRDO1, CRDO2 and General dropout)
			aape =tape/(dble(cyears-count(dropped))-dble(nmdrop)/12.0d0)	
		

	end function aape
		
	! Calculate cpp contribution
	double precision function tax(year, earn, selfearn,age, cpppensiondummy , qcdummy)
		integer, intent(in)	::year,age
		double precision, intent(in)	::earn,selfearn	
		! dummies for receiving cpp pensions and for qpp
		logical, intent(in) :: cpppensiondummy, qcdummy 
		double precision	:: contrearning, diff
		integer	:: t
		t= year-cppfirstyear+1
		if(age<18) then
		    tax= 0.0
		else if(qcdummy) then 
			! with qpp, there is no end of contributions when pension begins or with age
			! worker's contribution - not employer's
            tax = workcrate_qpp(t)*max(0.0, min(earn,ympe(t))-base(t))   
            ! selfearn contributory income on less the part on whivh contribution is already done as an employee
            tax = tax +  selfcrate_qpp(t) * max(0.0, min(ympe(t)-earn,selfearn-earn) - base(t) ) 
		else if(age >= 70 .or. cpppensiondummy) then
            tax = 0.0
        else
        	! worker's contribution - not employer's
            tax = workcrate(t)*max(0.0, min(earn,ympe(t))-base(t))   
            ! selfearn contributory income on less the part on whivh contribution is already done as an employee
            tax = tax +  selfcrate(t) * max(0.0, min(ympe(t)-earn,selfearn-earn) - base(t) ) 
		end if
					
	end function tax
	
	double precision function ben(aape,agecppstart,byear)
		integer, intent(in)	::agecppstart ,byear
		double precision, intent(in)	::aape	
		integer 					:: ycstart, ycstop, cyears
		integer	:: t, bt	
		ycstart = max(1966, byear+18)
		ycstop 	= byear + agecppstart
		cyears = ycstop-ycstart		
		t= ycstart - cppfirstyear + cyears +1 
		bt= byear-cppfirstbyear+1
	
		ben = reprate(t) *aape
		if(agecppstart>=nra(bt)) then
			ben = ben*(1.00 + drc(t) * min(lra(bt)-nra(bt), agecppstart - nra(bt) )	)
			!write(*,*) "getbenpaid1,0,",cppinc,",",agecppstart,",",nra(bt),",",&
			!drc(t),",",min(lra(bt)-nra(bt), agecppstart - nra(bt) ),",",reprate(t),",",aape
		else if (agecppstart<nra(bt) .and. agecppstart>=era(bt)) then
			ben = ben*(1.00 - arf(t) * min(nra(bt) - era(bt), nra(bt) - agecppstart ))	
		else 
			ben = 0.0d0
		end if	
	end function ben
	
	

	! !load cpp params from cpp.csv file
	! subroutine loadcpp_qppreform(DIRECTORY)
	
	! 	character(len=*) , intent(in)	:: DIRECTORY  	
	! 	integer 			::i, buffer
		
	! 	! Get numbers of parameters to import
	! 	open(1,file= '../params/cppyears_qppreform.csv')
	! 		read(1,*) cppfirstyear
	! 		read(1,*) cpplastyear
	! 		read(1,*) cppfirstyear_s1
	! 		read(1,*) cppfirstyear_s2
	
	! 	close(1)
				
	! 	ncppyears = cpplastyear-cppfirstyear+1

	! 	open(1,file= '../params/cppbyears_qppreform.csv')
	! 		read(1,*) cppfirstbyear
	! 		read(1,*) cpplastbyear
	! 	close(1)
				
	! 	ncppbyears=cpplastbyear-cppfirstbyear+1
		
	! 	! Parameters specific to current year
                        	
	! 	open(1,file= '../params/cppyear_qppreform.csv')
	! 	do i=1,ncppyears
	! 		read(1,*)buffer, ympe(i), ympe_s2(i), base(i), workcrate(i),empcrate(i),selfcrate(i),arf(i),drc(i), &
	! 		& nympe(i), reprate(i), reprate_s1(i), transit_rate(i), reprate_s2(i), gendroprate(i), & 
	! 		& workcrate_qpp(i),empcrate_qpp(i),selfcrate_qpp(i),&
	! 		& gendroprate_qpp(i), U1(i),U3(i),U4(i), maxcombined60(i), maxcombined65(i), &
	! 		& survagecutoff1(i), survagecutoff2(i), survrate1(i), survrate2(i), survrate_s(i)
	! 	end do
	! 	close(1)
		

	! 	! Parameters specific to birth year
	! 	allocate(era(ncppyears))
	! 	allocate(nra(ncppyears))
	! 	allocate(lra(ncppyears))
		
	! 	open(1,file= '../params/cppbyear_qppreform.csv')
	! 	do i=1,ncppbyears
	! 		read(1,*)buffer, era(i),nra(i),lra(i)
	! 	end do
	! 	close(1)

	! 	open(1,file= '../params/corr2010_curr.csv')
	! 	do i=1,50
	! 		read(1,*) corrcurr_2010(i) , corr2010_curr(i)
	! 	end do
	! 	close(1)
			
	! end subroutine loadcpp_qppreform
	
	! ! Find Avg (annual) Pensionable Earning with real rules following the steps of:
	! ! http://retirehappy.ca/how-to-calculate-your-cpp-retirement-pension/   
	! subroutine getaape_qppreform(aape,aape_s1,aape_s2,byear,year,age,cpphist, qcdummy,id)
		
	! 	integer, intent(in)		:: byear,id 	! birth year
	! 	integer, intent(in)		:: year	! current year 
	! 	integer, intent(in) 	:: age ! age at which cpp pension starts (60-70); should normally be current age
	! 	logical, intent(in)     :: qcdummy !true if qc (use qpp instead of cpp)
	! 	type(tp_cpphist), intent(in) ::cpphist 
		
	! 	double precision, intent(out)  ::aape, aape_s1, aape_s2 		! Total Adjusted Pensionable Earning 
				
	! 	integer 	:: ycstart, ycstart_s1, ycstart_s2, ycstop 	! years contribution period starts and year it end	
	! 	integer 	:: cyears, cyears_s1, cyears_s2		! number of contributory years
	! 	integer   :: nydrop, nydrop_s1, nydrop_s2		! number of years to drop for gen dropout
	! 	integer   :: nmdrop		! number of remaining months to drop (less than 12)
	! 	integer	:: ngendrop	! number of periods to drop for general dropout
		
	! 	integer	:: i,h, t , bt, p
	! 	integer	::j(1)		! need this to use minloc function
		
	! 	double precision::upe(:) , upe_s1(:), upe_s2(:)  ! Unadjusted Pensionable Earning
	! 	double precision::ape(:), ape_s1(:), ape_s2(:) ! Adjusted Pensionable Earning
	! 	logical , allocatable	:: dropped(:), dropped_s1(:), dropped_s2(:) ! .true. if period is dropped (disab-CRDP or general dropout)
	! 	double precision	::tape, tape_s1, tape_s2	 ! Total Adjusted Pensionable Earning 
	! 	double precision  ::avgympe ! Avg Max Pensionable Earning of the nympe last years
	! 	double precision  ::avgape 	 ! Avg  Adjusted Pensionable Earning
		
	! 	double precision:: earnh(58)=0.0d0
	! 	double precision:: earntest(58) = 0.0d0  
	! 	t= year-cppfirstyear+1
	! 	bt= byear-cppfirstbyear+1
			
	! 	! Change 2010 dollars in CPP history for current values 
		
	! 	do i = 1, 58
	! 		h =  max(1, min(2015-1966+1, byear+18-1966+i))
	! 		earnh(i) = 	cpphist%earnh(i) * corr2010_curr(h) 
	! 	end do
	! 	! Step 1: Calculate nb of contributory year (should be months for more precision)
			
	! 		!Contributory period begins either the month after you turn 18 or in January 1966, whichever is later
	! 		ycstart = max(1966, byear+18) 
	! 		ycstart_s1 = max(2019, byear+18) !For the first supplement, contributory period begeins the month after you turn 18 or in January 2019
	! 		ycstart_s2 = max(2024, byear+18) !For the second supplement, contributory period begins the month after you turn 18 or in January 2024

	! 		!It ends the month you turn 70 (lra) or the month before your CPP retirement pension starts, whichever is earlier. 
	! 		ycstop = min(byear+lra(bt),byear+age)
	! 		cyears= ycstop-ycstart							
	! 		cyears_s1 = ycstop - ycstart_s1
	! 		cyears_s2 = ycstop - ycstart_s2

	! 	! Step 2: Calculate Total Adjusted Pensionable Earning (TAPE)
			
	! 		allocate(upe(cyears))
	! 		allocate(upe_s1(cyears_s1))	
	! 		allocate(upe_s2(cyears_s2))		

	! 		allocate(ape(cyears))	
	! 		allocate(ape_s1(cyears_s1))	
	! 		allocate(ape_s2(cyears_s2))	

	! 		allocate(dropped(cyears))		
	! 		allocate(dropped_s1(cyears_s1))	
	! 		allocate(dropped_s2(cyears_s2))	

	! 		upe=-1.0d0
	! 		ape=-1.0d0
	! 		upe_s1=-1.0d0
	! 		ape_s1=-1.0d0
	! 		upe_s2=-1.0d0
	! 		ape_s2=-1.0d0
	! 		dropped(:) = .false.
	! 		dropped_s1(:) = .false.
	! 		dropped_s2(:) = .false.
				
	! 		!First calculate Unadjusted Pensionable Earning (UPE) for each contribution year			
	! 		!upe = max(0.0, min( earnh(1:cyears),ympe(t))-base(t))  !upe is the amount between base amount and max pen earn
	! 		! or  it is zero when earning is less than base
	! 		do i=1,cyears
	! 			upe(i) = min(earnh(i),ympe(year - cyears - cppfirstyear+i)) ! earnh(i) assumes contr period starts at 18 
	! 				if (upe(i) < base(year-cyears -cppfirstyear+i)) then
	! 				upe(i) = 0.0d0
	! 				end if
	! 		end do
			
	! 		!First calculate Unadjusted Pensionable Earning (UPE) for each contribution year for the first supplement			
	! 		do i=1,cyears_s1
	! 			p = cyears-cyears_s1 ! Need to start only on the last contribution years
	! 			upe_s1(i) = min(earnh(p+i),ympe(year - cyears_s1 - cppfirstyear+i)) ! earnh(i) assumes contr period starts at 18 
	! 				if (upe_s1(i) < base(year-cyears_s1 -cppfirstyear+i)) then
	! 				upe_s1(i) = 0.0d0
	! 				end if													
	! 		end do

	! 		!First calculate Unadjusted Pensionable Earning (UPE) for each contribution year for the second supplement			
	! 		!If earnings are smaller than YMPE of the basic CPP/QPP, than not eligible to the second supplement
	! 		do i=1,cyears_s2
	! 			p = cyears-cyears_s2 ! Need to start only on the last contribution years
	! 			upe_s2(i) = min(earnh(i+p),ympe_s2(year - cyears_s2 - cppfirstyear+i)) - ympe(year - cyears_s2 - cppfirstyear+i)! earnh(i) assumes contr period starts at 18 
	! 				if (earnh(i+p) < ympe(year-cyears_s2 -cppfirstyear+i)) then
	! 				upe_s2(i) = 0.0d0
	! 				end if		
			
	! 		end do


	! 		! check for diability benefit periods from 60 to 64 (era to nra) : only for the basic portion of the CPP
	! 		do i = era(bt),min(age-1 , nra(bt)-1)
	! 			if(cpphist%disabh(i-60+1))then	! check if disabled in arrays corresponding to yrs from ealy to before normal ret age 
	! 				upe(i-18+1 + min(0, byear-1948)) = 0.0	! exclude income from upe count 					
	! 				dropped(i-18+1 + min(0, byear-1948)) =  .true.
	! 			end if	
	! 		end do
			
			
	! 		! Find avg ympe in the five (nympe) last contr years ending in the yr cpp benefits start : same for both parts of the supplement
	! 		avgympe = sum(ympe(ycstop-nympe(t)+1-cppfirstyear+1: ycstop-cppfirstyear+1))/dble(nympe(t))		

	! 		! Calculate APE for each year: divide UPE by YMPE for each year and then multiply by avgympe : the supplements use the same adjustment value as the basic CPP/QPP
	! 		do  i = 1, size(ape)
	! 			ape(i) = upe(i)/ympe((year- cyears+count(dropped) -cppfirstyear+i))*avgympe 
	! 		end do

	! 		do  i = 1, size(ape_s1)
	! 			ape_s1(i) = upe_s1(i)/ympe((year- cyears_s1 -cppfirstyear+i))*avgympe 
	! 		end do

	! 		do  i = 1, size(ape_s2)
	! 			ape_s2(i) = upe_s2(i)/ympe((year- cyears_s2 -cppfirstyear+i))*avgympe 
	! 		end do

	! 		! TAPE is the sum of all APE
	! 		tape = sum(ape) 
	! 		tape_s1 = sum(ape_s1) 
	! 		tape_s2 = sum(ape_s2) 

	! 	! Step 3: Determine dropout periods	: only for basic CPP/QPP
			
	! 		! Child Rearing Drop Out Provision (CRDP) - see http://retirehappy.ca/child-rearing-dropout/
	! 		! Three steps: CRDO1, CRDO2 and general dropout  (disability already exlcuded frome cyears)
							
	! 		! CRDO1: Drop any period where children were under age 7 (if prim. caregiver) and earning was
	! 		! less than basic exemp (UPE=0)	
	! 			do  i = 1, cyears
	! 				if(upe(i) ==0.0d0 .and. cpphist%kid0_6h(i) ) then ! kidh(i) assumes contr period starts at 18			
	! 					dropped(i) = .true.	
	! 				end if		
	! 			end do	

	! 		!  Find avg APE - exlcuding CRDO1 dropout periods or disability years
	! 		avgape = 	tape/dble(cyears-count(dropped))
					
	! 		! CRDO2: Drop any period where child is under age 7 and APE is less than avg APE	
	! 			do  i = 1, cyears	
	! 				if(ape(i) <avgape .and. cpphist%kid0_6h(i)) then  ! kidh(i) assumes contr period starts at 18	
	! 					upe(i) =0.0d0
	! 					ape(i) =0.0d0
	! 					dropped(i) = .true.	
	! 				end if		
	! 			end do																	
				
	! 		! General dropout for basic CPP/QPP: Drop the X% lowest earning remaining periods.  Everyone's eligible.
			
	! 		if(qcdummy) then

 !                ! number of complete years (12 months) to drop
 !                nydrop = floor(gendroprate_qpp(t)*dble(cyears-count(dropped)))

 !                ! number of remaining months to drop
 !                 nmdrop = nint((gendroprate_qpp(t)*dble(cyears-count(dropped))-dble(floor(gendroprate_qpp(t)* &
 !                 & dble(cyears-count(dropped)))))*12)

	! 	    else
	! 			! number of complete years (12 months) to drop	
	! 			nydrop = floor(gendroprate(t)*dble(cyears-count(dropped)))
												
	! 			! number of remaining months to drop 
	! 			 nmdrop = nint((gendroprate(t)*dble(cyears-count(dropped))-dble(floor(gendroprate(t)* &
	! 			 & dble(cyears-count(dropped)))))*12)

 !            end if


	! 		! General dropout for the supplement : After 40 years of contribution, can replace lowest years by new ones. The 40 best years count in the calculation.
 !            ! number of complete years to drop
 !                nydrop_s1 = max(cyears_s1 - 40d0,0d0)
	! 			nydrop_s2 = max(cyears_s2 - 40d0,0d0)

	! 	    ! After-65-dropout (see Step 4 of http://retirehappy.ca/how-to-calculate-your-cpp-retirement-pension/)
				
	! 			if(age>nra(bt) .and. age < 70) then	
					
	! 				do i = nra(bt) , age-1  ! age -1 is the last contributory period
	! 					! If you're still working after 65 (and if these earnings are higher than avgAPE) , you can use these earning to replace any period under age 65
	! 					!write(*,*) age, byear , nra(bt)
	! 					if(ape(i-18+1 + min(0, byear-1948)) >= avgape) then 
	! 						nydrop = nydrop + 1 
						
	! 					! If you're not working after 65 or if earnings are less than avgAPE, you can drop periods after 65.
	! 					else
	! 						upe(i-18+1+ min(0, byear-1948)) =0.0
	! 						ape(i-18+1+ min(0, byear-1948)) =0.0
	! 						dropped(i-18+1+ min(0, byear-1948-1)) = .true.	
	! 					end if

	! 				end do
				
	! 			end if	
				
	! 		 ! Find yrs to be dropped (those with lowest income) : basic CPP/QPP
	! 		 	do i=1,nydrop
	! 		 		j = minloc(ape, .not. dropped)
	! 		 		upe(j) = 0.0
	! 		 		ape(j) = 0.0
	! 		 		dropped(j) = .true.				
	! 		 	end do 
				 	 
	! 		 	if(nmdrop>0) then	! If we have to find a year to partly exclude 
	! 				j = minloc(ape, .not. dropped)
	! 				upe(j) = (1.0d0-dble(nmdrop)/dble(12)) * upe(j)
	! 		 		ape(j) = (1.0d0-dble(nmdrop)/dble(12)) * ape(j) 	 								
	! 			end if
		
	! 		 ! Find yrs to be dropped (those with lowest income) : supplement CPP/QPP
	! 		 	do i=1,nydrop_s1
	! 		 		j = minloc(ape_s1, .not. dropped_s1)
	! 		 		upe_s1(j) = 0.0
	! 		 		ape_s1(j) = 0.0
	! 				dropped_s1(j) = .true.		
	! 		 	end do 

	! 			do i=1,nydrop_s2
	! 		 		j = minloc(ape_s2, .not. dropped_s2)
	! 		 		upe_s2(j) = 0.0
	! 		 		ape_s2(j) = 0.0
	! 				dropped_s2(j) = .true.	
	! 		 	end do 
		
	! 	! Step 4: Calculate Average Annual (should be Monthly) Pensionable Earning (AAPE)	
			
	! 		! Recalculate after-dropout TAPE
	! 		tape = sum(ape) 
	! 		tape_s1 = sum(ape_s1) 
	! 		tape_s2 = sum(ape_s2) 

	! 		! AAPE - divide by cyears minus all dropped years (disab, CRDO1, CRDO2 and General dropout)
	! 		aape =tape/(dble(cyears-count(dropped))-dble(nmdrop)/12.0d0)	

	! 		! AAPE - divide by 40 years for the supplement
	! 		aape_s1 =transit_rate(t) * tape_s1/40d0
	! 		aape_s2 =tape_s2/40d0

 !    	!put back to 2010 dollars
	! 	aape = aape*corrcurr_2010(min(t,50))
	! 	aape_s1 = aape_s1*corrcurr_2010(min(t,50))
	! 	aape_s2 = aape_s2*corrcurr_2010(min(t,50))

	! end subroutine getaape_qppreform
		
	
	! subroutine getbenpaid_qppreform(cppinc, cppinc_b, cppinc_s, aape, aape_s1, aape_s2, agecppstart,year,byear, id)
	! 	integer, intent(in)	::agecppstart ,year, byear, id
	! 	double precision, intent(in)	:: aape, aape_s1, aape_s2
	! 	double precision, intent(out) :: cppinc, cppinc_b, cppinc_s
	! 	double precision			  :: cppinc_s1, cppinc_s2
	! 	integer	:: t, bt
	! 	integer 					:: ycstart, ycstop, cyears
		
	! 	!Besoin du nombre d'années de cotisations pour trouver l'année où l'individu commence à recevoir ses prestations
	! 	!C'est cette année là qui doit être utilisée pour trouver le bon tau de remplacement ainsi que les taux pour retraite anticipée/repoussée (arf/drc)
	! 	ycstart = max(1966, byear+18)
	! 	ycstop 	= byear + agecppstart
	! 	cyears = ycstop-ycstart	 			

	! 	t= ycstart - cppfirstyear + cyears +1 !Veut l'année où l'individu commence à recevoir ses prestations
	! 	bt= byear-cppfirstbyear+1
	
	! 	cppinc_b = reprate(t) *aape
	! 	cppinc_s1 = reprate_s1(t) * aape_s1
	! 	cppinc_s2 = reprate_s2(t) * aape_s2

 !        !Rules for early and late claiming are the same for basic CPP/QPP and for the supplements
	! 	if(agecppstart>=nra(bt)) then
	! 		cppinc_b = cppinc_b*(1.00 + drc(t) * min(lra(bt)-nra(bt), agecppstart - nra(bt) ))
	! 		cppinc_s1 = cppinc_s1*(1.00 + drc(t) * min(lra(bt)-nra(bt), agecppstart - nra(bt) )	)
	! 		cppinc_s2 = cppinc_s2*(1.00 + drc(t) * min(lra(bt)-nra(bt), agecppstart - nra(bt) )	)
	! 	end if
		
	! 	if(agecppstart<nra(bt)) then
	! 		cppinc_b = cppinc_b*(1.00 - arf(t) * min(nra(bt) - era(bt), nra(bt) - agecppstart ))
	! 		cppinc_s1 = cppinc_s1*(1.00 - arf(t) * min(nra(bt) - era(bt), nra(bt) - agecppstart ))	
	! 		cppinc_s2 = cppinc_s2*(1.00 - arf(t) * min(nra(bt) - era(bt), nra(bt) - agecppstart ))		

	! 	end if

	! 	cppinc = cppinc_b+cppinc_s1+cppinc_s2
	! 	cppinc_s = cppinc_s1+cppinc_s2
	! 	end subroutine getbenpaid_qppreform

	
	! subroutine survpension(cppinc, survpeninc, spcppinc,age,agecppstart, combine_year,survyear,byear, children,id)
	! 	integer, intent(in)::	age,agecppstart,combine_year,survyear, byear,id
	! 	logical, intent(in)::	children ! dummy, true if the person lives with one or more children
	! 	double precision, intent(inout) :: spcppinc, cppinc ! rente rrq du conjoint decede
	! 	double precision, intent(inout) :: survpeninc ! IN: rente de retraite rrq de la personne; OUT: rente retraite+survivant
	! 	double precision :: UP !Prestations uniformes : valeur change selon l'âge du conjoint survivant
	! 	double precision :: AF ! Facteur d'ajustement pour retraite anticipée et repoussée
	! 	double precision :: totinc
	! 	integer :: agecppstart_temp
	! 	integer	:: t, bt, ct,mt
	! 	t= 2010-cppfirstyear+1 !Comme les prestations uniformes sont indexées à l'inflation, on veut toujours leur valeur de 2010
	! 	bt= byear-cppfirstbyear+1 ! Pour aller chercher early, normal et late retirement age dans le fichier params/cppbyear
	! 	ct = survyear - cppfirstyear+1   !année où la personne commence à recevoir sa rente de conjoint survivant
	! 	mt = combine_year - cppfirstyear+1  !Comme le maximum de la rente combinée est indexée aux salaires, on veut l'année de combinaison de
	! 										! la rente rrq et de la pension du conjoint survivant	

	! 	!Pour les personnes qui ont déjà une rente de RRQ en 2010, agecppstart = -1. Par contre, on calcule tout de même leurs prestations de conjoint survivant
	! 	!J'ai choisi de considérer que leur agecppstart était de 65 ans pour le calcul de la rente combinée (Aurélie, 22-03-2018)

	! 	agecppstart_temp = agecppstart

	! 	if (agecppstart .eq. -1) then 
	! 		agecppstart_temp = 65
	! 	end if

	! !Calcul de la rente survivant
	! 	if(age<survagecutoff1(ct))then 
	! 		if(children)then
	! 			UP = U3(t)
	! 			survpeninc = spcppinc*survrate1(ct)+UP

	! 		else
	! 			UP = U4(t)
	! 			survpeninc = spcppinc*survrate1(ct)+UP			

	! 		end if

	! 	else if (age<survagecutoff2(ct)) then
	! 		UP = U1(t)
	! 		survpeninc = spcppinc*survrate1(ct)+UP		
	! 	else
			
	! 		survpeninc = spcppinc*survrate2(ct)	
	! 	end if	

	! !Calcul de la rente combinee
	! if (combine_year .gt. 0 .and. age<65)  then
	! 	AF = arf(mt) * min(nra(bt)-era(bt), nra(bt) -agecppstart_temp )
	! 	if (cppinc + survpeninc .gt. UP + (maxcombined60(mt) * (1.0d0 -  AF)) ) then
	! 		survpeninc = UP + (maxcombined60(mt) * (1.0d0 -  AF)) - cppinc
	! 	end if	

	! else if (combine_year .gt. 0 .and. age .ge. 65) then
	! 	AF = drc(mt) * min(lra(bt)-nra(bt), agecppstart_temp - nra(bt))
	! 	totinc = min(max((spcppinc + cppinc)*survrate2(mt), cppinc + (spcppinc * survrate1(mt))), maxcombined65(mt)*(1.0d0 + AF))
	! 	survpeninc = totinc - cppinc

	! else !Pas de rente combinée pour les personnes qui n'ont pas de combine_year, seulement la pension du conjoint survivant
	! 	cppinc = survpeninc
	! end if

	! end subroutine survpension
	
	! subroutine survpension_qppreform(cppinc,spcppinc, survpeninc, spcppinc_b, spcppinc_s,age,&
	! & agecppstart,combine_year,survyear,byear, children, id)
	! 	integer, intent(in)::	age,agecppstart,combine_year,survyear,byear, id
	! 	logical, intent(in)::	children ! dummy, true if the person lives with one or more children
	! 	double precision, intent(in) :: spcppinc, spcppinc_s ! rente rrq du conjoint decede
	! 	double precision, intent(inout) :: cppinc,spcppinc_b ! IN: rente de retraite rrq de la personne; OUT: rente retraite+survivant
	! 	double precision, intent(inout) :: survpeninc
	! 	double precision :: UP !Prestations uniformes : valeur change selon l'âge du conjoint survivant
	! 	double precision :: AF, totinc ! Facteur d'ajustement pour retraite anticipée et repoussée
	! 	integer :: agecppstart_temp
	! 	integer	:: t, bt, ct,mt

	! 	t= 2010-cppfirstyear+1 !Comme les prestations uniformes sont indexées à l'inflation, on veut toujours leur valeur de 2010
	! 	bt= byear-cppfirstbyear+1 ! Pour aller chercher early, normal et late retirement age dans le fichier params/cppbyear
	! 	ct = survyear - cppfirstyear+1   !année où la personne commence à recevoir sa rente de conjoint survivant
	! 	mt = combine_year - cppfirstyear+1  !Comme le maximum de la rente combinée est indexée aux salaires, on veut l'année de combinaison de
	! 										! la rente rrq et de la pension du conjoint survivant	

	! 	!Pour les personnes qui ont déjà une rente de RRQ en 2010, agecppstart = -1. Par contre, on calcule tout de même leurs prestations de conjoint survivant
	! 	!J'ai choisi de considérer que leur agecppstart était de 65 ans pour le calcul de la rente combinée (Aurélie, 22-03-2018)
	! 	agecppstart_temp = agecppstart

	! 	if (agecppstart .eq. -1) then 
	! 		agecppstart_temp = 65
	! 	end if

	! 	!Pour les conjoints qui n'étaient pas dans la réforme, la totalité de leurs revenus de RRQ sont en fait la partie de RRQ de base de la réforme
	! 	if (spcppinc_b .eq. 0) then 
	! 		spcppinc_b = spcppinc
	! 	end if 


	! !Calcul de la rente survivant

	! 	if(age<survagecutoff1(ct))then 
			
	! 		if(children)then
	! 			UP = U3(t)
	! 			survpeninc = spcppinc_b*survrate1(ct)+UP
			
	! 		else
	! 			UP = U4(t)
	! 			survpeninc = spcppinc_b*survrate1(ct)+UP			
			
	! 		end if
			
	! 	else if(age<survagecutoff2(ct))then
	! 		UP = U1(t)
	! 		survpeninc = spcppinc_b*survrate1(ct)+UP
		
	! 	else
			
	! 		survpeninc = spcppinc_b*survrate2(ct)
			
	! 	end if
			
	! 	! Pour le supplément du RPC/RRQ , ajoute simplement une portion du revenu de pension 
	! 	survpeninc = survpeninc + spcppinc_s* survrate_s(ct) 
	
	! !Calcul de la rente combinee
	! if (combine_year .gt. 0 .and. age<65) then
	! 	AF = arf(mt) * min(nra(bt)-era(bt), nra(bt) -agecppstart_temp)
	! 	if (cppinc + survpeninc .gt. UP + (maxcombined60(mt) * (1.0d0 -  AF)) ) then
	! 		survpeninc = UP + (maxcombined60(mt) * (1.0d0 -  AF)) - cppinc
	! 	end if	

	! else if (combine_year .gt. 0 .and. age .ge. 65) then
	! 	AF = drc(mt) * min(lra(bt)-nra(bt), agecppstart_temp - nra(bt))
	! 	totinc = min(max((spcppinc + cppinc)*survrate2(mt), cppinc + (spcppinc * survrate1(mt))), maxcombined65(mt)*(1.0d0 + AF))
	! 	survpeninc = totinc - cppinc

	! else !Pas de rente combinée pour les personnes qui n'ont pas de combine_year, seulement la pension du conjoint survivant
	! 	cppinc = survpeninc
	! end if


	! end subroutine survpension_qppreform
end module cpp
	
