
    
    PROGRAM LIB
    
    USE MOD_FACT
    USE STRINGS
    
    REAL*8 DVAL, DVAL1
    REAL*8 DGAMMA
    REAL*16 QVAL,QVAL1
    
    INTEGER(4) :: IRES, IVAR=23
    INTEGER(8) :: IIRES, IIVAR=23
    REAL(4) :: SRES, SVAR=23.0
    REAL(8) :: DRES, DVAR=23.D0
    REAL(16) :: QRES, QVAR=23.Q0
    
        
    DVAL=DGAMMA(1.2D0)
    DVAL1=GAMMA(1.2D0)
    
    WRITE(*,*)DVAL
    WRITE(*,*)DVAL1
    
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
    
    PAUSE
    
    END