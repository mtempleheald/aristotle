module QualificationElement
( 
    QE(..)-- export all value constructors for QE data type
) 
where
--import Control.Parallel

-- Define new datatype with "data", not interchangeable with a different enum type or string
data QeType = Scheme | Award | Pathway | LearningUnit | Assessable
              deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- Assessable < Scheme -- This example returns False because of the enumerated list order (not technically relevant but interesting)

-- Define synonyms for existing datatypes with "type", just used for clarity
type PartyId = String
type AwardingOrganisationId = PartyId
type QeId = String 

data QeKey = QeKey { AwardingOrganisationId :: AwardingOrganisationId
                   , Type :: QeType
                   , Id :: QeId
                   } deriving (Eq, Show, Read)
-- derive Eq so that we can check 2 QEs are identical (OK since Strings and Int are both equatable)
-- only equal if all three fields are equal
-- derive Show to get a string representation of our type
-- derive Read to be able to convert a string representation to our type, need to tell Haskell how (which type to use)
read "QeKey {AwardingOrganisationId = \"AQA\", Type=\"Assessable\", Id=\"ENG2H\"}" :: QeKey

data QE = QE { Key :: QeKey
             , Name :: String
             , ShortName :: String
             } deriving (Eq, Show, Read)

describeQE :: QE -> String
describeQE (QE {AwardingOrganisationId = ao, Type = t, Id = id}) = "This " ++ ao ++ " QE is of type " ++ t ++ " and has id " ++ id 

let EngGCSEAward = QE {AwardingOrganisationId="AQA", Type="Award", Id="ENG1F"}
describeQE EngGCSEAward