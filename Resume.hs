{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Resume where

import           Data.Aeson (ToJSON, FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BS
import           GHC.Generics

data Resume = Resume
    { contact           :: ContactInfo
    , education         :: [Education]
    , technicalSkills   :: Skills
    , workExperience    :: [WorkExperience]
    , projects          :: [Project]
    } deriving (Generic, ToJSON, FromJSON)

data ContactInfo = ContactInfo
    { address   :: String
    , email     :: String
    , website   :: String
    , github    :: String
    } deriving (Generic, ToJSON, FromJSON)

data School = School
    { schoolName      :: String
    , schoolLocation  :: String
    } deriving (Generic, ToJSON, FromJSON)

data Education = Education
    { school                :: School
    , graduationDate        :: String
    , gpa                   :: Maybe String
    , graduatingAchievement :: Maybe String
    , scholarships          :: Maybe [String]
    , otherAchievements     :: Maybe [String]
    } deriving (Generic, ToJSON, FromJSON)

data Skills = Skills
    { proficient    :: [String]
    , familiar      :: [String]
    } deriving (Generic, ToJSON, FromJSON)

data WorkExperience = WorkExperience
    { company   :: Company
    , positions :: [Position]
    } deriving (Generic, ToJSON, FromJSON)

data Company = Company
    { name              :: String
    , location          :: String
    , companyWebsite    :: Maybe String
    } deriving (Generic, ToJSON, FromJSON)

data Position = Position
    { title     :: String
    , date      :: String
    , details   :: [String]
    } deriving (Generic, ToJSON, FromJSON)

data Project = Project
    { projectTitle  :: String
    , role          :: String
    , timeframe     :: String
    , description   :: String
    } deriving (Generic, ToJSON, FromJSON)

-------------------------------------------------------------------------------
-- Helper Functions --
-------------------------------------------------------------------------------

getResume :: IO Resume
getResume = do
    json <- BS.readFile "json/resume.json"
    case eitherDecode json of
        Left  err -> error err
        Right val -> return val

