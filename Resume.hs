{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Resume
    ( Resume (..)
    , resumeCtx
    , getResume
    ) where

import           Data.Aeson           (ToJSON, FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BS
import           GHC.Generics
import           Control.Monad        (sequence)
import           Control.Applicative  (empty)
import           Data.Monoid          (mconcat)
import           Hakyll

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
-- Hakyll Resume Context --
-------------------------------------------------------------------------------

resumeCtx :: Resume -> Context String
resumeCtx resume = mconcat . map ($ resume) $
    [ contactCtx . contact
    , educationCtx . education
    , workExperienceCtx . workExperience
    ]

contactCtx :: ContactInfo -> Context String
contactCtx contactInfo = mconcat . map ($ contactInfo) $
    [ constField "address" . address
    , constField "email" . email
    , constField "website" . website
    , constField "github" . github
    ]


educationCtx :: [Education] -> Context String
educationCtx educationList =
    listField "education"
        (mconcat
            [ field "schoolName" (return . schoolName . school . itemBody)
            , field "schoolLocation" (return . schoolLocation . school . itemBody)
            , field "gpa" (maybe empty return . gpa . itemBody)
            , field "graduatingAchievement" (maybe empty return . graduatingAchievement . itemBody)
            , listFieldWith "scholarships"
                (bodyField "scholarship")
                (maybe empty (sequence . map makeItem) . scholarships . itemBody)
            , listFieldWith "otherAchievements"
                (bodyField "achievement")
                (maybe empty (sequence . map makeItem) . otherAchievements . itemBody)
            ]
        )
        (sequence . map makeItem $ educationList)

workExperienceCtx :: [WorkExperience] -> Context String
workExperienceCtx workExperienceList =
    listField "workExperience"
        (mconcat
            [ field "companyName" (return . name . company . itemBody)
            , field "companyLocation" (return . location . company . itemBody)
            , field "companyWebsite" (maybe empty return . companyWebsite . company . itemBody)
            , listFieldWith "positions"
                (mconcat
                    [ field "positionTitle" (return . title . itemBody)
                    , field "positionDate" (return . date . itemBody)
                    , listFieldWith "positionDetails"
                        (bodyField "detail")
                        (sequence . map makeItem . details . itemBody)
                    ]
                )
                (sequence . map makeItem . positions . itemBody)
            ]
        )
        (sequence . map makeItem $ workExperienceList)
-------------------------------------------------------------------------------
-- Helper Functions --
-------------------------------------------------------------------------------

getResume :: IO Resume
getResume = do
    json <- BS.readFile "json/resume.json"
    case eitherDecode json of
        Left  err -> error err
        Right val -> return val

