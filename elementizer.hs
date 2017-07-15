-- Usage:
--   $ ./elementizer esther
--   Es Th Er

import Data.Char (isLetter, toLower)
import Data.Function.Memoize (memoFix)
import Data.List (intercalate, isPrefixOf)
import System.Environment (getArgs)

elements = [ "H"  , "He" , "Li" , "Be" , "B"  , "C"  , "N"  , "O"  , "F"
           , "Ne" , "Na" , "Mg" , "Al" , "Si" , "P"  , "S"  , "Cl" , "Ar"
           , "K"  , "Ca" , "Sc" , "Ti" , "V"  , "Cr" , "Mn" , "Fe" , "Co"
           , "Ni" , "Cu" , "Zn" , "Ga" , "Ge" , "As" , "Se" , "Br" , "Kr"
           , "Rb" , "Sr" , "Y"  , "Zr" , "Nb" , "Mo" , "Tc" , "Ru" , "Rh"
           , "Pd" , "Ag" , "Cd" , "In" , "Sn" , "Sb" , "Te" , "I"  , "Xe"
           , "Cs" , "Ba" , "La" , "Ce" , "Pr" , "Nd" , "Pm" , "Sm" , "Eu"
           , "Gd" , "Tb" , "Dy" , "Ho" , "Er" , "Tm" , "Yb" , "Lu" , "Hf"
           , "Ta" , "W"  , "Re" , "Os" , "Ir" , "Pt" , "Au" , "Hg" , "Tl"
           , "Pb" , "Bi" , "Po" , "At" , "Rn" , "Fr" , "Ra" , "Ac" , "Th"
           , "Pa" , "U"  , "Np" , "Pu" , "Am" , "Cm" , "Bk" , "Cf" , "Es"
           , "Fm" , "Md" , "No" , "Lr" , "Rf" , "Db" , "Sg" , "Bh" , "Hs"
           , "Mt" , "Ds" , "Rg" , "Cn" , "Uut", "Uuq", "Uup", "Uuh", "Uus"
           , "Uuo" ]

elementize = memoFix (\fn str ->
  if null str
    then return []
    else do element <- filterPrefixes elements str
            soln <- fn $ drop (length element) str
            return $ element : soln)
  where toLowerStr = map toLower
        filterPrefixes prefixes str = filter
          (\x -> isPrefixOf (toLowerStr x) (toLowerStr str))
          prefixes

main = do args <- getArgs
          putStrLn $ intercalate"\n"  $ map (intercalate " ")
            (elementize $ filter isLetter $ concat args)
