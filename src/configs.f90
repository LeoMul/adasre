module configs 
    implicit none 
    !LMX(I) IS THE NO OF DISTINCT OPEN-SHELL ORBITALS IN CONFIG I.
    !QSB(I,J) IS THE OCCUPATION NO OF ORBITAL J IN CONFIG I.
    !QLB(I,J) IS THE ORBITAL NO OF ORBITAL J IN CONFIG I, J=1,LMX(I).
    !QS0,QL0 CONTAIN EISSNER SPECIFICATION OF CONFIG TO BE DECODED.
    !ICF(I) IS THE CONFIG NO OF CONFIG I IN THE MASTER LIST.
    integer,parameter :: shelldim      = 40 
    integer,parameter :: cfdim         = 200
    integer,parameter :: totalshelldim = 40 
    character*19      :: cflabel(cfdim)
    integer           :: princN(totalshelldim)
    integer           :: orbL  (totalshelldim)
    integer           :: NII(cfdim)
    
    integer           :: QS0    (shelldim)
    character*1       :: QL0    (shelldim)
    character*4       :: QL0TEMP(shelldim)
    character*4       :: char4Temp
    character*1       :: char1Temp

    character*1       :: MBLNK =' '
    integer           :: NGR,MA0,MB0 
    character*8       :: char8
    character*1       :: QSB(cfdim,shelldim)
    integer           :: QLB(cfdim,shelldim)
    integer           :: QMB(cfdim,shelldim)
    integer           :: LMX(cfdim)

    integer           :: icf(shelldim)
    integer           :: mxocc(shelldim)
    integer           :: k,m,mlast
    integer           :: imatch,MXORBR,NLIT=60
    CHARACTER*1       :: COR(5),LIT(0:21),LAB1
    CHARACTER*3       :: ORBLAB(36)
    integer           :: i,J

    character*1       :: QSB_CONT(cfdim,shelldim)
    integer           :: QLB_CONT(cfdim,shelldim)
    integer           :: QMB_CONT(cfdim,shelldim)
    integer           :: LMX_CONT(cfdim)
    integer           :: ncontium_cf = 0 

    character*1       :: QSB_CONT_UNIQUE(cfdim,shelldim)
    integer           :: QLB_CONT_UNIQUE(cfdim,shelldim)
    integer           :: QMB_CONT_UNIQUE(cfdim,shelldim)
    integer           :: LMX_CONT_UNIQUE(cfdim)
    integer           :: ncontium_cf_UNIQUE = 0 
    logical :: check1,check2
    character*1 :: LLAB(0:11)
    character*1 :: OCCLAB(0:14)
    DATA LLAB/'S','P','D','F','G','H','I','K','L','M','N','O'/
    DATA LIT /'0','1','2','3','4','5','6','7','8','9', &
              'A','B','C','D','E','F','G','H','I','J', &
              'K','L'/
    DATA OCCLAB/'0','1','2','3','4','5','6','7','8','9','A','B','C',& 
    'D','E'/
    DATA ORBLAB /' 1S', &
                 ' 2S',' 2P',&
                 ' 3S',' 3P',' 3D',&
                 ' 4S',' 4P',' 4D',' 4F',&
                 ' 5S',' 5P',' 5D',' 5F',' 5G',&
                 ' 6S',' 6P',' 6D',' 6F',' 6G',' 6H',&
                 ' 7S',' 7P',' 7D',' 7F',' 7G',' 7H',' 7I',&
                 ' 8S',' 8P',' 8D',' 8F',' 8G',' 8H',' 8I',' 8K'&
                 /
    contains 

    subroutine decode_eissner(ncf,formatted)
        !this code is partially lifted from Nigel Badnell's ADASDR 
        !I have not refactored the go to's

        !The part to decode Eissner is from adasDR 
        !The part which isolates the continuum portion is original 
        !That part has a goto in it - for ease of use
        !Its use is fairly clear - but i should probably do better with 
        !it.

        implicit none 
        integer           :: ncf 
        integer           :: ncontium_cf
        integer           :: iostat
        logical           :: formatted 


179     FORMAT(2I5,2X,I3,I2,1X,10(I2,A1))
        ncontium_cf = 0 
        QLB = 0
        QMB = 0 
        QSB = MBLNK
        open(20,file='configs')

        do i = 1,ncf 
            !Here I simply check the formatted-ness of the file at every 
            !read. For the configurations we are only doing of order 
            !10 - 100 configurations, and I can live with that.
            !Additionally, unlike the Auger reads - this read is fairly 
            !nuanced. and i'd rather more maintainable code at the cost 
            !of an immeasurably small performance cost.
            if (formatted) then
            !for a formatted file, life is easy, life can be dream,
            !as the kids say. 
                READ(1,179,end=1002)NII(I),NGR,MA0,MB0,&
                                    (QS0(J),QL0(J),J=1,10)
            else 
            !In unformatted, the Eissner notation is stored in 
            !integer*4. This is precisely 3 more bytes than necessary.
            !I need to read this in as a character*4 - and then 
            !cast down to a character*1 which is what I actually need.
                READ(1,iostat=iostat,end=1002)NII(I),NGR,MA0,MB0,&
                                    (QS0(J),QL0TEMP(J),J=1,10)
                do j = 1,10 
                    char4Temp = QL0TEMP(J)
                    char1Temp(1:1) = char4Temp(1:1)
                    QL0(J) = char1Temp
                end do 



            end if 
            write(20,*) 'DEBUG',i,QL0(1)
            

            DO 16 J=1,10
                QSB(I,J)=MBLNK
                IF(QL0(J).EQ.MBLNK)GO TO 16
                LMX(I)=J
                M=MOD(QS0(J),50)
                QMB(I,J)=M
                mlast=m
                IF(M.GT.0)QSB(I,J)=LIT(M)
                DO K=1,size(LIT)
                  !WRITE(20,*) QL0(J),LIT(K)
                  IF(QL0(J).EQ.LIT(K)) then 
                    !WRITE(20,*)'going to 19'
                    GO TO 19
                  end if 
                ENDDO
                QLB(I,J)=0
                GO TO 16
        19      QLB(I,J)=K
                MXORBR=MAX(MXORBR,K)
                IF(NII(I).LT.0)THEN
                  IF(IMATCH.eq.0.OR.IMATCH.EQ.-I) then 
                    MXOCC(K)=MAX(M,MXOCC(K))             !so IMATCH.lT.0
                  END IF 
                ELSE
                QMB_CONT(I,J) = QMB(I,J)
                  IF(IMATCH.EQ.-I)MXOCC(K)=MAX(M,MXOCC(K))
                ENDIF
        16  ENDDO
        write(20,'(10(I2,A1))') (QS0(J),QL0(J),J=1,lmx(i))
        !write(20,*) QSB(I,:)
        !write(20,*) QLB(I,1:lmx(i))
        
        if (NII(I) .LT. 0 ) then 
            !we are in a continuum state. ignore the last occupation. 
            ncontium_cf=ncontium_cf+1
            LMX_CONT(ncontium_cf) = lmx(i)-1
            QSB_CONT(ncontium_cf,:) = MBLNK
            QLB_CONT(ncontium_cf,:) = 0 
            QMB_CONT(ncontium_cf,:) = 0 
            do j = 1, LMX_CONT(ncontium_cf)
                QSB_CONT(ncontium_cf,J) = QSB(I,J)
                QLB_CONT(ncontium_cf,J) = QLB(I,J)
                QMB_CONT(ncontium_cf,J) = QMB(I,J)
            end do 
            write(20,*) 'Debug Cont'
            write(20,*) 
            !write(20,*) QLB_CONT(I,1:LMX_CONT(i)),QSB_CONT(I,:)
            write(20,*) (QLB_CONT(ncontium_cf,J), &
                     QSB_CONT(ncontium_cf,J),J=1,LMX_CONT(ncontium_cf)) 
        end if 


        end do 

        QSB_CONT_UNIQUE(:,:) = MBLNK
        QLB_CONT_UNIQUE(:,:) = 0 
        QMB_CONT_UNIQUE(:,:) = 0 

        QSB_CONT_UNIQUE(1,:) = QSB_CONT(1,:)
        QLB_CONT_UNIQUE(1,:) = QLB_CONT(1,:)
        QMB_CONT_UNIQUE(1,:) = QMB_CONT(1,:)
        LMX_CONT_UNIQUE(1)   = LMX_CONT(1)
        ncontium_cf_UNIQUE = 1
        write(20,*) 'First cont'
        write(20,*) (QLB_CONT(1,J),QSB_CONT(1,J),J=1,LMX_CONT(1)) 

        do i = 2, ncontium_cf 
            do j = 1 , ncontium_cf_UNIQUE 

                check1 = compareTwoArraysCha(QSB_CONT(I,:),& 
                                              QSB_CONT_UNIQUE(J,:))
                check2 = compareTwoArraysInt(QLB_CONT(I,:),& 
                                              QLB_CONT_UNIQUE(J,:))

                if ( check1.AND.check2) THEN
                !i know i know its a goto, but its actually readable ok
                    go to 1 
                end if
            end do 

            ncontium_cf_UNIQUE = ncontium_cf_UNIQUE+1
            QSB_CONT_UNIQUE(ncontium_cf_UNIQUE,:) = QSB_CONT(I,:)
            QLB_CONT_UNIQUE(ncontium_cf_UNIQUE,:) = QLB_CONT(I,:)
            QMB_CONT_UNIQUE(ncontium_cf_UNIQUE,:) = QMB_CONT(I,:)
            LMX_CONT_UNIQUE(ncontium_cf_UNIQUE) = LMX_CONT(I)
            1 continue 
        end do 
        write(20,*) 'i found ',ncontium_cf_UNIQUE
        do i = 1,ncontium_cf_UNIQUE 
            write(20,*) i 
            write(20,*) QSB_CONT_UNIQUE(i,1:LMX_CONT_UNIQUE(I))
            write(20,*) QLB_CONT_UNIQUE(i,1:LMX_CONT_UNIQUE(I))
        end do 
        close(20)
        
        return 

        1002 continue
        stop ' error, in reading configs in decode_eissner'
        !I know, it's a go to. I inherited this one from NRB. 
    end subroutine

    function compareTwoArraysInt(A,B) 
        !compares two arrays of integers 
        !If they have index with not the same element, return false.
        implicit none
        logical :: compareTwoArraysInt 
        integer :: A(:),B(:)
        integer :: ii 
        compareTwoArraysInt = .true. 
        do ii = 1, size(A)
            if ( A(ii).ne.B(ii)) then 
                compareTwoArraysInt  = .false.
                return
            end if 
        end do 
    end function

    function compareTwoArraysCha(A,B) 
        !compares two arrays of characters*1's 
        !If they have index with not the same element, return false.
        implicit none
        logical :: compareTwoArraysCha 
        character*1 :: A(:),B(:)
        integer :: ii 
        compareTwoArraysCha = .true. 
        do ii = 1, size(A)
            if ( A(ii).ne.B(ii)) then 
                compareTwoArraysCha  = .false.
                return
            end if 
        end do 
    end function

end module configs 