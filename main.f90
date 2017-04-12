
    
    PROGRAM LIB
    
    USE MOD_FACT
    !USE MOD_STRINGS
    
    INTEGER(4) :: IRES, IVAR=8
    INTEGER(8) :: IIRES, IIVAR=8
    REAL(4) :: SRES, SVAR=8.0
    REAL(8) :: DRES, DVAR=8.D0
    REAL(16) :: QRES, QVAR=8.Q0
    REAL(4) :: SSRES
    REAL(8)::DSRES
    REAL(16)::QSRES
    
    REAL*8 DVAL, DVAL1
    REAL*8 DGAMMA
    REAL*16 QVAL,QVAL1
         
    IRES=FACT(IVAR)
    WRITE(*,*)"     ",IVAR,"    ",IRES
    IIRES=FACT(IIVAR)
    WRITE(*,*)"     ",IIVAR,"    ",IIRES
    SRES=FACT(SVAR)
    WRITE(*,*)PRECISION(SVAR),"     ",SVAR,"    ",SRES
    DRES=FACT(DVAR)
    WRITE(*,*)PRECISION(DVAR),"     ",DVAR,"    ",DRES
    QRES=FACT(QVAR)
    WRITE(*,*)PRECISION(QVAR),"     ",QVAR,"    ",QRES
    SSRES=SEMIFACT(SVAR)
    WRITE(*,*)PRECISION(SVAR),"     ",SVAR,"    ",SSRES
    DSRES=SEMIFACT(DVAR)
    WRITE(*,*)PRECISION(DVAR),"     ",DVAR,"    ",DSRES
    QSRES=SEMIFACT(QVAR)
    WRITE(*,*)PRECISION(DVAR),"     ",QVAR,"    ",QSRES
        
    WRITE(*,*)'**********************************************************************'
    
    DVAL=DGAMMA(1.2D0)
    DVAL1=GAMMA(1.2D0)
    
    WRITE(*,*)DVAL
    WRITE(*,*)DVAL1
    
    PAUSE
    
    END