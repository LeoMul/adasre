module kinds 
    !It is the users responsbility to correctly manage their types, as 
    !it depends on their method of using autostructure. 
    !
    use iso_fortran_env, only:      readInt =>  int32
    use iso_fortran_env, only:    levelIter =>  int32
    use iso_fortran_env, only: resonantIter =>  int64
    use iso_fortran_env, only:    readFloat => real64
    use iso_fortran_env, only:    floatKind => real64
    use iso_fortran_env, only:    largeIter =>  int64

    !
    !In autostructure, no matter the integer kind you have compiled it with.
    !The unformatted write-outs have integer values, say intval,
    ! casted to int(intval). I.e, with the default integer kind for your
    !compiler. This is almost always integer*4 (int32). 
    !Therefore, reads from the oicu files have to have int32 precision. **
    !However, there can in principle be more AugerRates than are 
    !countable in int64. Therefore iterators over the resonances need to be
    !int64. The levelITers can remain int32, as at some point I put things
    !into rank-2 errors, so neither index will exceed int32 capabilities - 
    !unless the number of levels in a block exceeds 2^31 -1. 

    !** However! If you have compiled autostructure using -i8 with say the intel 
    !compiler, then there is a further level of nuance. In this circumstance,
    !the casting function int(intval) will absolutely cast to i8, int64.
    !This is because this flag updates the default value of the kind paramemter
    !within the int() caster.
    !In this case, the other kind must be updated here to int64. **
end module 