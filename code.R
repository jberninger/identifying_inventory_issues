# Asset Management and HAVA Inventory Problem
#############################################

# master dataset = only the am_equipment_log file
# hava = hava1 + hava2 + pbr/abb - (duplicates)

## REPEATED INVENTORY NUMBERS IN AM DATASET

# Frequency table of those NOT in HAVA
## has the inventory number, description, model, serial number and DATES for AM enteries

# Frequency table of those in HAVA
## has the inventory number, description, model, serial number and DATES for AM and HAVA enteries
## also includes a source field

##############################################
# load the files. dont need the inactive file
setwd("E:/JordanBerninger/AM_IT_Inventory (Blanca, Fulbright)")
library(DT)
library(data.table)
library(dplyr)
library(readr)
# load the data
am <- read_csv("E:/JordanBerninger/AM_IT_Inventory (Blanca, Fulbright)/AM_EQUIPMENT_INVENTORY_LOG.csv")
pbr <- read_csv("E:/JordanBerninger/AM_IT_Inventory (Blanca, Fulbright)/AM_PBR_ABB.csv")
hava <- read_csv("E:/JordanBerninger/AM_IT_Inventory (Blanca, Fulbright)/AM_HAVA.csv")

# clean the columns and combine pbr + hava into the new hava

am <- am %>% select("inventory" = 7, "serial" = 6, "division" = 1, "manager" = 2,  "description" = 4, "model" = 5, "notes" = 9, 
                    "purchase_date" = 3, "issue_date" = 10, "salvage_date" = 15)

pbr <- pbr %>% select("inventory" = 7, "serial" = 6, "division" = 1, "manager" = 2,  "description" = 4, "model" = 5, "notes" = 12, 
                      "purchase_date" = 3, "issue_date" = 9, "salvage_date" = 11)

hava <- hava %>% select("inventory" = 7, "serial" = 6, "division" = 1, "manager" = 2,  "description" = 4, "model" = 5, "notes" = 13, 
                        "purchase_date" = 3, "issue_date" = 9, "salvage_date" = 11)

hava <- rbind(pbr, hava)

# check the str() of the date fields, correct if necessary
# correct all the date field formats below
## this could be problematic if PBR and HAVA had different date formats for the same column, but they didnt :)

hava <- hava %>% mutate(purchase_date = as.Date(purchase_date, "%m/%d/%Y"), 
                        issue_date = as.Date(issue_date, "%m/%d/%Y"), salvage_date = as.Date(salvage_date, "%m/%d/%Y"))

am <- am %>% mutate(purchase_date = gsub(" ", "", purchase_date)) %>%
  mutate(purchase_date = as.Date(purchase_date, "%m/%d/%Y"), 
         issue_date = as.Date(issue_date, "%d-%b-%y"), 
         salvage_date = as.Date(salvage_date, "%d-%b-%y"))
# remove some white space, use the POSIXct date format
############################################################################################
# make the new description column that maps to larger groups
# consider this 'description buckets'
# this is to correct for typos, differeent syntaxes, etc, etc, 
# lots of noise in the original description field because it was hand-entered
############################################################################################


am <- am %>% mutate(jrb_desc = description)
hava <- hava %>% mutate(jrb_desc = description)
# overwrite it (for the purposes of not losing any data)
am$jrb_desc[am$description == "FAN 16''"] <- "FAN 16\""
am$jrb_desc[am$description == "MONITOR 19''"] <- "MONITOR 19\""
am$jrb_desc[am$description %in% c("HAND HELD SCANNER", "IMAGE SCANNER",
                                  "HAND HELD - SCANNER",
                                  "SCANNER HANDHELD")] <- "MOBILE SCANNER"
am$jrb_desc[am$description %in% c("WALKIE TALKIE","WALKIE-TALKIE", 
                                  "WALKIE TALKE")] <- "TWO-WAY RADIO"
am$jrb_desc[am$description %in% c("COPIER/FAX MACHINE")] <- "FAX MACHINE/COPIER"
am$jrb_desc[am$description %in% c("20\" MONITOR", "MONITOR 20''")] <- "MONITOR 20\""
am$jrb_desc[am$description %in% c("CD/DVD REWRITER", "DVD/VHS PLAYER" , 
                                  "DVD R-BURNER","DVD/CD BURNER", "CD BURNER", "CD/DVD BURNER", 
                                  "CD/DVD DUPLICATOR", "DVD PRINT FACTORY", "DVD BURNER PANASONIC",
                                  "DVD DUPLICATOR") ] <- "CD/DVD BURNER"
am$jrb_desc[am$description %in% c("3 HOLE PUNCH", 
                                  "2 HOLE PUNCH", "ELECTRIC 3-HOLE PUNCH", 
                                  "PUNCH & STAPLER" , "ELECTRIC PUNCH", "HOLE PUCH","HOLE PUNCH",
                                  "ELECTRIC PUNCH STAPLER" )] <- "HOLE PUNCHER"
am$jrb_desc[am$description %in% c("TV/DVD 21''", "TV/DVD 21\"")] <- "MONITOR 21\""
am$jrb_desc[am$description %in% c("BARCODE SCANNER", "SCANNER GUN")] <- "LASER SCANNER"
am$jrb_desc[am$description %in% c("LABELING ELECTRIC SYSTEM", "LABLE WRITER",
                                  "LABEL MANAGER", "LABEL PRINTER",                      
                                  "LABEL WRITER" ,  "LABELING PRINTER",                   
                                  "LABELING SYSTEM" )] <- "LABEL MAKER"
am$jrb_desc[am$description %in% c("MONITOR 21.3''")] <- "MONITOR 21.3\""
am$jrb_desc[am$description %in% c("DIGITAL LEVEL")] <- "DIGITAL LEVELER"  
am$jrb_desc[am$description %in% c("CELLPHONE W/BLUETOOTH HEADSET")] <- "CELL PHONE W/BLUETOOTH HEADSET" 
am$jrb_desc[am$description %in% c("MANUAL EMBOSSER")] <- "EMBOSSER"
am$jrb_desc[am$description %in% c("WIRELESS RECIEVER" )] <- "WIRELESS RECEIVER"
am$jrb_desc[am$description %in% c("TRANSMITTER/BODY PACK" , 
                                  "PLUG-IN  TRANSMITTER", "WIRELESS MIC TRANSIMITTER", 
                                  "WIRELESS MIC TRANSMITTER", "PLUG-IN TRANSMITTER", 
                                  "WIRELESS TRANSMITTER", 
                                  "PLUG-IN-TRANSMITTER")] <- "TRANSMITTER"
am$jrb_desc[am$description %in% c("SIGNTURE PAD")] <- "SIGNATURE PAD"
am$jrb_desc[am$description %in% c("MONITOR  24\"", "MONITOR 24\"", 
                                  "MONIROR 24\"", "MONIGOR 24\"", 
                                  "MONITOR 24'", "MONITOR \"24", 
                                  "24\"MONITOR SAMSUNG", "24\"SAMSUNG MONITOR", 
                                  "24\"MONITOR", "24\" MONITOR",  "MONITOR  24''",
                                  "24\"MONITORS", "MONITOR 24''")] <- "MONITOR  24\""
am$jrb_desc[am$description %in% c("KEYBOARD VIDEO AND MONITOR")] <- "KEYBOARD AND MONITOR"
am$jrb_desc[am$description %in% c("PLANTROICS WIRELESS HEADSET" )] <- "PLANTRONICS WIRELESS HEADSET"
am$jrb_desc[am$description %in% c("MONITOR 42\" LCD", "TELEVISION 42\"")] <- "TV 42\"" 
am$jrb_desc[am$description %in% c("MEGAPHONE ( BULLHORN) 15W", 
                                  "MEGAPHONE 15W", 
                                  "MEGAPHONE (BULLHORN) 15W")] <- "MEGAPHONE"
am$jrb_desc[am$description %in% c("WEB CAM")] <- "WEBCAM" 
am$jrb_desc[am$description %in% c("SCANNER CURRENCY", "CURRANCY COUNTER")] <- "CURRENCY COUNTER" 
am$jrb_desc[am$description %in% c("SIG PADS", 
                                  "SIG PAD", "SIGNATURE  PAD", 
                                  "SIGNATURE CAPTURE MADULE")] <- "SIGNATURE PAD"
am$jrb_desc[am$description %in% c("MULTIFUNCTION SPEAKER")] <- "MULTIFUNCTION SPEAKERS"
am$jrb_desc[am$description %in% c("27\" MONITOR CONFIGURATION", 
                                  "MONIITOR 27\"", "MONITOR 27\" CONFIGURATION 7", 
                                  "MONITOR 27\"", "MONITOR \"27", "MONITOR 27''", 
                                  "27'' MONITOR")] <- "MONITOR  27\""
am$jrb_desc[am$description %in% c("DIVERSITY RECIEVER SLX4")] <- "DIVERSITY RECEIVER SLX4"
am$jrb_desc[am$description %in% c("DEWALT DRILL" )] <- "DRILL"
am$jrb_desc[am$description %in% c("DIGITAL VCR", 
                                  "DIGITAL VIDEO RECORDER", "DIGITAL MINI VCR", 
                                  "DIGTAL MINI VCR", "DIGITAL  MINI  VCR",
                                  "VCR 3/4 INCH TAPE")] <- "VCR"
am$jrb_desc[am$description %in% c("GPS NETWORK FLEET VEHICLE #61289",
                                  "GPS NETWORK FLEET VEHICLE #61292",
                                  "GPS NETWORK FLEET VEHICLE #61290",
                                  "GPS NETWORK FLEET VEHICLE #60423",  
                                  "GPS NETWORK FLEET VEHICLE #60248",   
                                  "GPS NETWORK FLEET VEHICLE #60421",   
                                  "GPS NETWORK FLEET VEHICLE #61291",   
                                  "GPS NETWORK FLEET VEHICLE #60422",
                                  "GPS NETWORK FLEET VEHICLE #61295",
                                  "GPS NETWORK FLEET VEHICLE #33366")] <- "GPS NETWORK FLEET VEHICLE"
am$jrb_desc[am$description %in% c("LD PROJECTOR")] <- "LCD PROJECTOR" 
am$jrb_desc[am$description %in% c("CHARING CRADLE BARCODE SCANNER", 
                                  "CHARGE CRADLE BARCODE SCANNER" )] <- "CHARGING CRADLE BARCODE SCANNER"
am$jrb_desc[am$description %in% c("8 PORT SWITCH DESKTOP")] <- "8-PORT SWITCH DESKTOP" 
am$jrb_desc[am$description %in% c("AIR CROMPRESSOR")] <- "AIR COMPRESSOR"
am$jrb_desc[am$description %in% c("BALLOT CARD READER")] <- "BALLOT READER" 
am$jrb_desc[am$description %in% c("BAR CODE READER", "BAR CODE SCANNER" )] <- "BARCODE READER" 
am$jrb_desc[am$description %in% c("BLACBERRY", "BLACK BERRY", "BLACKBERRY CURVE 3G",
                                  "BLACKBERRY CURVE 9360", "BLACKBERRY W/ BLUETOOTH", "BLACKBERRY W/BLUETOOTH")] <-   "BLACKBERRY"
am$jrb_desc[am$description %in% c("BROADBAND CARD")] <- "BROADBAND WIRELESS CARD"
am$jrb_desc[am$description %in% c("CACULATOR" )] <- "CALCULATOR"
am$jrb_desc[am$description %in% c("CELL PHONE W/BLUETOOTH HEADSET", "CELLULAR PHONE" )] <-   "CELL PHONE"
am$jrb_desc[am$description %in% c("CPU / MONITOR ALL IN ONE")] <- "CPU"  
am$jrb_desc[am$description %in% c("DATE STAMP", "TIME DATE STAMP","TIME STAMP", "TIMESTAMP", "TME STAMP")] <- "DATE/TIME STAMP"
am$jrb_desc[am$description %in% c("DOOR PRESSURE GAGE", "DOOR PRESSURE GUAGE")] <- "DOOR PRESSURE GAUGE"
am$jrb_desc[am$description %in% c("DVD RECORDERS")] <- "DVD RECORDER"
am$jrb_desc[am$description %in% c("ELECTRIC SHARPENER", "ELECTRIC PENCIL SHARPNER" )] <- "ELECTRIC PENCIL SHARPENER"
am$jrb_desc[am$description %in% c("FAN 12 \"")] <- "FAN 12\"" 
am$jrb_desc[am$description %in% c("FAN 20''" )] <- "FAN 20\"" 
am$jrb_desc[am$description %in% c("FAX MACHINE", "FAX-COPIER MACHINE")] <- "FAX MACHINE/COPIER"
am$jrb_desc[am$description %in% c("FILM SPLICERS", "FILM SLICER" )] <- "FILM SPLICER"
am$jrb_desc[am$description %in% c("HEAD SET")] <- "HEADSET"
am$jrb_desc[am$description %in% c("HOLE PUNCHER" )] <- "HOLE PUNCH"
am$jrb_desc[am$description %in% c("HUB SUPER STACK II")] <- "HUB SUPERSTACK II"
am$jrb_desc[am$description %in% c("LETTER OPENER, ELECTRIC", "MAIL OPENER" )] <- "LETTER OPENER"
am$jrb_desc[am$description %in% c("MONITOR 15''")] <- "MONITOR 15\""
am$jrb_desc[am$description %in% c("MONITOR 17''" )] <- "MONITOR 17\""
am$jrb_desc[am$description %in% c("MONITOR 20'")] <- "MONITOR 20\"" 
am$jrb_desc[am$description %in% c("MONITOR 21''")] <- "MONITOR 21\"" 
am$jrb_desc[am$description %in% c("MONITOR 22''" , "MONITOR 22\" B2240W")] <-  "MONITOR 22\""
am$jrb_desc[am$description %in% c( "PALLET JACK CROWN LIFT TRUCK MANUAL",
                                   "PALLET JACK MANUAL",                 
                                   "PALLET JACK/ELECTRIC")] <- "PALLET JACK" 
am$jrb_desc[am$description %in% c("PENCIL SHARPNER" , "PENCIL ELECTRIC SHARPENER", "SHARPENER", "SHARPNER")] <- "PENCIL SHARPENER"
am$jrb_desc[am$description %in% c("PROJECTION SCREEN" )] <- "PROJECTOR SCREEN"
am$jrb_desc[am$description %in% c("RECEIPT  PRINTER" , "RECIEPT PRINTER")] <- "RECEIPT PRINTER" 
am$jrb_desc[am$description %in% c("RECEIVER/BODY PACK" )] <- "RECEIVER/BODYPACK" 
am$jrb_desc[am$description %in% c("REFRIDGERATOR", "REFRIDGERATOR/FREEZER", "REFRIGERATOR 21 CU FT.")] <- "REFRIGERATOR"
am$jrb_desc[am$description %in% c("REMOTE POINT  GLOBAL PRESENTER")] <- "REMOTE POINT GLOBAL PRESENTER"
am$jrb_desc[am$description %in% c("SAMSUNG GALAXY S6", "SAMSUNG GALAXY S6 EDGE")] <- "SAMSUNG GALAXY"
am$jrb_desc[am$description %in% c("SCALE, COUNTING", "SCALE, POSTAL, ALL PURPOSE")] <- "SCALE"
am$jrb_desc[am$description %in% c("SHEDDER", "SHEET-CROSSCUT SHREDDER")] <- "SHREDDER"
am$jrb_desc[am$description %in% c("SPEAKERS")] <- "SPEAKER"
am$jrb_desc[am$description %in% c("TRANSMITTER BODY PACK")] <- "TRANSMITTER BODYPACK"
am$jrb_desc[am$description %in% c("TV/DVD 20''","TV/DVD/VCR","TV/DVD/VCR 27\"","TV/VCR",                             
                                  "TV/VCR/DVD 27\"")] <- "TV/VCR/DVD"
am$jrb_desc[am$description %in% c("TYPEWRITTER")] <- "TYPEWRITER"
am$jrb_desc[am$description %in% c("USB/VGA - TRANSFORMER/PROJECTOR")] <- "USB/VGA TRANSFORMER/PROJECTOR"
am$jrb_desc[am$description %in% c("WIRELESS MICCROPHONE")] <- "WIRELESS MICROPHONE"
am$jrb_desc[am$description %in% c("WIRELESS MIC RECEIVER", "WIRELESS MIC", 
                                  "WIRELESS  MIC TRANSMITTER" )] <-"WIRELESS MICROPHONE"
am$jrb_desc[am$description %in% c("5-PORT SWITCH DESKTOP" )] <- "5-PORT SWITCH" 
am$jrb_desc[am$description %in% c("ANCHOR  SPEAKER", "ANCHOR POWERED MON SPEAKER")] <- "ANCHOR SPEAKER"
am$jrb_desc[am$description %in% c("APPLE COUMPUTER DUAL PROCESSOR")] <- "APPLE CPU" 
am$jrb_desc[am$description %in% c("CAMERA  CANON POWERSHOT")] <- "CANON POWERSHOT CAMERA"
am$jrb_desc[am$description %in% c("CANON AUTO FOCUS ZOOM LENS")] <- "CANON AUTO FOCUS LENS"
am$jrb_desc[am$description %in% c("CANON SPEEDLITE")] <- "CANON FLASH SPEEDLITE"
am$jrb_desc[am$description %in% c("CART PLATFORM MED.","CART PLATFORM SM.","CART- HAND PULL",
                                  "CART-HAND PULL","CART, PLASTIC 18X24X42",
                                  "CART, UTILITY, 2 SHELVES")] <- "CART"
am$jrb_desc[am$description %in% c("DA-LITE 60\"INSTA-THEATER","DA-LITE 80\"INSTA-THEATER")] <- "DA-LITE 60\" SCREEN"
am$jrb_desc[am$description %in% c("DALITE 120\"ADVANTAGE ELECTRICAL")] <- "DALITE 120\" SCREEN"
am$jrb_desc[am$description %in% c("ELECTRIC STAPLER BLACK/STANLEY" , "ELETRIC STAPLER", "ELECTRONIC STAPLER")] <- "ELECTRIC STAPLER"
am$jrb_desc[am$description %in% c("HEADSET CORDLS W/ADAPTOR", "HEADSET CORDLESS")] <-  "CORDLESS TELEPHONE HEADSET" 
am$jrb_desc[am$description %in% c("MAGNECTIC STRIPE READER")] <- "MAGNECTIC STRIP READER"
am$jrb_desc[am$description %in% c("MICRO VIEWER")] <- "MICROVIEWER"
am$jrb_desc[am$description %in% c("MOBILE COMPUTER")] <- "LAPTOP"
am$jrb_desc[am$description %in% c("MONITOR 13''")] <- "MONITOR 13\""
am$jrb_desc[am$description %in% c("MONITOR 20'' PL2010M-BK" )] <- "MONITOR 20\""
am$jrb_desc[am$description %in% c("MONITOR 21'", "MONITOR 21\" LCD")] <- "MONITOR 21\""
am$jrb_desc[am$description %in% c("SHERDDER")] <- "SHREDDER"
am$jrb_desc[am$description %in% c("SIGNATURE CAPTURE")] <- "SIGNATURE PAD"
am$jrb_desc[am$description %in% c("SPEAKER SYSTEM / AMP")] <- "SPEAKER"
am$jrb_desc[am$description %in% c("STAPLER ELECTRIC","STAPLER, HEAVY DUTY")] <- "STAPLER"
am$jrb_desc[am$description %in% c("TEN KEYPAD")] <- "TEN KEY PAD"
am$jrb_desc[am$description %in% c("TELEVISION 27''")] <- "TELEVISION 27\"" 
am$jrb_desc[am$description %in% c("TV 20\"")] <- "TELEVISION 20\""
am$jrb_desc[am$description %in% c("TV 50\" LED")] <- "TELEVISION 50\""
am$jrb_desc[am$description %in% c("TV 42\"")] <- "TELEVISION 42\""
am$jrb_desc[am$description %in% c("VIDEO CAMERA ADAPTOR")] <-"VIDEO CAMERA ADAPTER"
am$jrb_desc[am$description %in% c("VIDEO CAMERA W LENS", "VIDEO CAMERA  W LENS")] <- "VIDEO CAMERA W/ LENS"
##----------------------------------------------------------------------------
am$jrb_desc[am$description == "FAN 16''"] <- "FAN 16\""
am$jrb_desc[am$description == "MONITOR 19''"] <- "MONITOR 19\""
am$jrb_desc[am$description %in% c("HAND HELD SCANNER", "IMAGE SCANNER",
                                  "HAND HELD - SCANNER",
                                  "SCANNER HANDHELD")] <- "MOBILE SCANNER"
am$jrb_desc[am$description %in% c("WALKIE TALKIE","WALKIE-TALKIE", 
                                  "WALKIE TALKE")] <- "TWO-WAY RADIO"
am$jrb_desc[am$description %in% c("COPIER/FAX MACHINE")] <- "FAX MACHINE/COPIER"
am$jrb_desc[am$description %in% c("20\" MONITOR", "MONITOR 20''")] <- "MONITOR 20\""
am$jrb_desc[am$description %in% c("CD/DVD REWRITER", "DVD/VHS PLAYER" , 
                                  "DVD R-BURNER","DVD/CD BURNER", "CD BURNER", "CD/DVD BURNER", 
                                  "CD/DVD DUPLICATOR", "DVD PRINT FACTORY", "DVD BURNER PANASONIC",
                                  "DVD DUPLICATOR") ] <- "CD/DVD BURNER"
am$jrb_desc[am$description %in% c("3 HOLE PUNCH", 
                                  "2 HOLE PUNCH", "ELECTRIC 3-HOLE PUNCH", 
                                  "PUNCH & STAPLER" , "ELECTRIC PUNCH", "HOLE PUCH","HOLE PUNCH",
                                  "ELECTRIC PUNCH STAPLER" )] <- "HOLE PUNCHER"
am$jrb_desc[am$description %in% c("TV/DVD 21''", "TV/DVD 21\"")] <- "MONITOR 21\""
am$jrb_desc[am$description %in% c("BARCODE SCANNER", "SCANNER GUN")] <- "LASER SCANNER"
am$jrb_desc[am$description %in% c("LABELING ELECTRIC SYSTEM", "LABLE WRITER",
                                  "LABEL MANAGER", "LABEL PRINTER",                      
                                  "LABEL WRITER" ,  "LABELING PRINTER",                   
                                  "LABELING SYSTEM" )] <- "LABEL MAKER"
am$jrb_desc[am$description %in% c("MONITOR 21.3''")] <- "MONITOR 21.3\""
am$jrb_desc[am$description %in% c("DIGITAL LEVEL")] <- "DIGITAL LEVELER"  
am$jrb_desc[am$description %in% c("CELLPHONE W/BLUETOOTH HEADSET")] <- "CELL PHONE W/BLUETOOTH HEADSET" 
am$jrb_desc[am$description %in% c("MANUAL EMBOSSER")] <- "EMBOSSER"
am$jrb_desc[am$description %in% c("WIRELESS RECIEVER" )] <- "WIRELESS RECEIVER"
am$jrb_desc[am$description %in% c("TRANSMITTER/BODY PACK" , 
                                  "PLUG-IN  TRANSMITTER", "WIRELESS MIC TRANSIMITTER", 
                                  "WIRELESS MIC TRANSMITTER", "PLUG-IN TRANSMITTER", 
                                  "WIRELESS TRANSMITTER", 
                                  "PLUG-IN-TRANSMITTER")] <- "TRANSMITTER"
am$jrb_desc[am$description %in% c("SIGNTURE PAD")] <- "SIGNATURE PAD"
am$jrb_desc[am$description %in% c("MONITOR  24\"", "MONITOR 24\"", 
                                  "MONIROR 24\"", "MONIGOR 24\"", 
                                  "MONITOR 24'", "MONITOR \"24", 
                                  "24\"MONITOR SAMSUNG", "24\"SAMSUNG MONITOR", 
                                  "24\"MONITOR", "24\" MONITOR",  "MONITOR  24''",
                                  "24\"MONITORS", "MONITOR 24''")] <- "MONITOR  24\""
am$jrb_desc[am$description %in% c("KEYBOARD VIDEO AND MONITOR")] <- "KEYBOARD AND MONITOR"
am$jrb_desc[am$description %in% c("PLANTROICS WIRELESS HEADSET" )] <- "PLANTRONICS WIRELESS HEADSET"
am$jrb_desc[am$description %in% c("MONITOR 42\" LCD", "TELEVISION 42\"")] <- "TV 42\"" 
am$jrb_desc[am$description %in% c("MEGAPHONE ( BULLHORN) 15W", 
                                  "MEGAPHONE 15W", 
                                  "MEGAPHONE (BULLHORN) 15W")] <- "MEGAPHONE"
am$jrb_desc[am$description %in% c("WEB CAM")] <- "WEBCAM" 
am$jrb_desc[am$description %in% c("SCANNER CURRENCY", "CURRANCY COUNTER")] <- "CURRENCY COUNTER" 
am$jrb_desc[am$description %in% c("SIG PADS", 
                                  "SIG PAD", "SIGNATURE  PAD", 
                                  "SIGNATURE CAPTURE MADULE")] <- "SIGNATURE PAD"
am$jrb_desc[am$description %in% c("MULTIFUNCTION SPEAKER")] <- "MULTIFUNCTION SPEAKERS"
am$jrb_desc[am$description %in% c("27\" MONITOR CONFIGURATION", 
                                  "MONIITOR 27\"", "MONITOR 27\" CONFIGURATION 7", 
                                  "MONITOR 27\"", "MONITOR \"27", "MONITOR 27''", 
                                  "27'' MONITOR")] <- "MONITOR  27\""
am$jrb_desc[am$description %in% c("DIVERSITY RECIEVER SLX4")] <- "DIVERSITY RECEIVER SLX4"
am$jrb_desc[am$description %in% c("DEWALT DRILL" )] <- "DRILL"
am$jrb_desc[am$description %in% c("DIGITAL VCR", 
                                  "DIGITAL VIDEO RECORDER", "DIGITAL MINI VCR", 
                                  "DIGTAL MINI VCR", "DIGITAL  MINI  VCR",
                                  "VCR 3/4 INCH TAPE")] <- "VCR"
am$jrb_desc[am$description %in% c("GPS NETWORK FLEET VEHICLE #61289",
                                  "GPS NETWORK FLEET VEHICLE #61292",
                                  "GPS NETWORK FLEET VEHICLE #61290",
                                  "GPS NETWORK FLEET VEHICLE #60423",  
                                  "GPS NETWORK FLEET VEHICLE #60248",   
                                  "GPS NETWORK FLEET VEHICLE #60421",   
                                  "GPS NETWORK FLEET VEHICLE #61291",   
                                  "GPS NETWORK FLEET VEHICLE #60422",
                                  "GPS NETWORK FLEET VEHICLE #61295",
                                  "GPS NETWORK FLEET VEHICLE #33366")] <- "GPS NETWORK FLEET VEHICLE"
am$jrb_desc[am$description %in% c("LD PROJECTOR")] <- "LCD PROJECTOR" 
am$jrb_desc[am$description %in% c("CHARING CRADLE BARCODE SCANNER", 
                                  "CHARGE CRADLE BARCODE SCANNER" )] <- "CHARGING CRADLE BARCODE SCANNER"
am$jrb_desc[am$description %in% c("8 PORT SWITCH DESKTOP")] <- "8-PORT SWITCH DESKTOP" 
am$jrb_desc[am$description %in% c("AIR CROMPRESSOR")] <- "AIR COMPRESSOR"
am$jrb_desc[am$description %in% c("BALLOT CARD READER")] <- "BALLOT READER" 
am$jrb_desc[am$description %in% c("BAR CODE READER", "BAR CODE SCANNER" )] <- "BARCODE READER" 
am$jrb_desc[am$description %in% c("BLACBERRY", "BLACK BERRY", "BLACKBERRY CURVE 3G",
                                  "BLACKBERRY CURVE 9360", "BLACKBERRY W/ BLUETOOTH", "BLACKBERRY W/BLUETOOTH")] <-   "BLACKBERRY"
am$jrb_desc[am$description %in% c("BROADBAND CARD")] <- "BROADBAND WIRELESS CARD"
am$jrb_desc[am$description %in% c("CACULATOR" )] <- "CALCULATOR"
am$jrb_desc[am$description %in% c("CELL PHONE W/BLUETOOTH HEADSET", "CELLULAR PHONE" )] <-   "CELL PHONE"
am$jrb_desc[am$description %in% c("CPU / MONITOR ALL IN ONE")] <- "CPU"  
am$jrb_desc[am$description %in% c("DATE STAMP", "TIME DATE STAMP","TIME STAMP", "TIMESTAMP", "TME STAMP")] <- "DATE/TIME STAMP"
am$jrb_desc[am$description %in% c("DOOR PRESSURE GAGE", "DOOR PRESSURE GUAGE")] <- "DOOR PRESSURE GAUGE"
am$jrb_desc[am$description %in% c("DVD RECORDERS")] <- "DVD RECORDER"
am$jrb_desc[am$description %in% c("ELECTRIC SHARPENER", "ELECTRIC PENCIL SHARPNER" )] <- "ELECTRIC PENCIL SHARPENER"
am$jrb_desc[am$description %in% c("FAN 12 \"")] <- "FAN 12\"" 
am$jrb_desc[am$description %in% c("FAN 20''" )] <- "FAN 20\"" 
am$jrb_desc[am$description %in% c("FAX MACHINE", "FAX-COPIER MACHINE")] <- "FAX MACHINE/COPIER"
am$jrb_desc[am$description %in% c("FILM SPLICERS", "FILM SLICER" )] <- "FILM SPLICER"
am$jrb_desc[am$description %in% c("HEAD SET")] <- "HEADSET"
am$jrb_desc[am$description %in% c("HOLE PUNCHER" )] <- "HOLE PUNCH"
am$jrb_desc[am$description %in% c("HUB SUPER STACK II")] <- "HUB SUPERSTACK II"
am$jrb_desc[am$description %in% c("LETTER OPENER, ELECTRIC", "MAIL OPENER" )] <- "LETTER OPENER"
am$jrb_desc[am$description %in% c("MONITOR 15''")] <- "MONITOR 15\""
am$jrb_desc[am$description %in% c("MONITOR 17''" )] <- "MONITOR 17\""
am$jrb_desc[am$description %in% c("MONITOR 20'")] <- "MONITOR 20\"" 
am$jrb_desc[am$description %in% c("MONITOR 21''")] <- "MONITOR 21\"" 
am$jrb_desc[am$description %in% c("MONITOR 22''" , "MONITOR 22\" B2240W")] <-  "MONITOR 22\""
am$jrb_desc[am$description %in% c( "PALLET JACK CROWN LIFT TRUCK MANUAL",
                                   "PALLET JACK MANUAL",                 
                                   "PALLET JACK/ELECTRIC")] <- "PALLET JACK" 
am$jrb_desc[am$description %in% c("PENCIL SHARPNER" , "PENCIL ELECTRIC SHARPENER", "SHARPENER", "SHARPNER")] <- "PENCIL SHARPENER"
am$jrb_desc[am$description %in% c("PROJECTION SCREEN" )] <- "PROJECTOR SCREEN"
am$jrb_desc[am$description %in% c("RECEIPT  PRINTER" , "RECIEPT PRINTER")] <- "RECEIPT PRINTER" 
am$jrb_desc[am$description %in% c("RECEIVER/BODY PACK" )] <- "RECEIVER/BODYPACK" 
am$jrb_desc[am$description %in% c("REFRIDGERATOR", "REFRIDGERATOR/FREEZER", "REFRIGERATOR 21 CU FT.")] <- "REFRIGERATOR"
am$jrb_desc[am$description %in% c("REMOTE POINT  GLOBAL PRESENTER")] <- "REMOTE POINT GLOBAL PRESENTER"
am$jrb_desc[am$description %in% c("SAMSUNG GALAXY S6", "SAMSUNG GALAXY S6 EDGE")] <- "SAMSUNG GALAXY"
am$jrb_desc[am$description %in% c("SCALE, COUNTING", "SCALE, POSTAL, ALL PURPOSE")] <- "SCALE"
am$jrb_desc[am$description %in% c("SHEDDER", "SHEET-CROSSCUT SHREDDER")] <- "SHREDDER"
am$jrb_desc[am$description %in% c("SPEAKERS")] <- "SPEAKER"
am$jrb_desc[am$description %in% c("TRANSMITTER BODY PACK")] <- "TRANSMITTER BODYPACK"
am$jrb_desc[am$description %in% c("TV/DVD 20''","TV/DVD/VCR","TV/DVD/VCR 27\"","TV/VCR",                             
                                  "TV/VCR/DVD 27\"")] <- "TV/VCR/DVD"
am$jrb_desc[am$description %in% c("TYPEWRITTER")] <- "TYPEWRITER"
am$jrb_desc[am$description %in% c("USB/VGA - TRANSFORMER/PROJECTOR")] <- "USB/VGA TRANSFORMER/PROJECTOR"
am$jrb_desc[am$description %in% c("WIRELESS MICCROPHONE")] <- "WIRELESS MICROPHONE"
am$jrb_desc[am$description %in% c("WIRELESS MIC RECEIVER", "WIRELESS MIC", 
                                  "WIRELESS  MIC TRANSMITTER" )] <-"WIRELESS MICROPHONE"
am$jrb_desc[am$description %in% c("5-PORT SWITCH DESKTOP" )] <- "5-PORT SWITCH" 
am$jrb_desc[am$description %in% c("ANCHOR  SPEAKER", "ANCHOR POWERED MON SPEAKER")] <- "ANCHOR SPEAKER"
am$jrb_desc[am$description %in% c("APPLE COUMPUTER DUAL PROCESSOR")] <- "APPLE CPU" 
am$jrb_desc[am$description %in% c("CAMERA  CANON POWERSHOT")] <- "CANON POWERSHOT CAMERA"
am$jrb_desc[am$description %in% c("CANON AUTO FOCUS ZOOM LENS")] <- "CANON AUTO FOCUS LENS"
am$jrb_desc[am$description %in% c("CANON SPEEDLITE")] <- "CANON FLASH SPEEDLITE"
am$jrb_desc[am$description %in% c("CART PLATFORM MED.","CART PLATFORM SM.","CART- HAND PULL",
                                  "CART-HAND PULL","CART, PLASTIC 18X24X42",
                                  "CART, UTILITY, 2 SHELVES")] <- "CART"
am$jrb_desc[am$description %in% c("DA-LITE 60\"INSTA-THEATER","DA-LITE 80\"INSTA-THEATER")] <- "DA-LITE 60\" SCREEN"
am$jrb_desc[am$description %in% c("DALITE 120\"ADVANTAGE ELECTRICAL")] <- "DALITE 120\" SCREEN"
am$jrb_desc[am$description %in% c("ELECTRIC STAPLER BLACK/STANLEY" , "ELETRIC STAPLER", "ELECTRONIC STAPLER")] <- "ELECTRIC STAPLER"
am$jrb_desc[am$description %in% c("HEADSET CORDLS W/ADAPTOR", "HEADSET CORDLESS")] <-  "CORDLESS TELEPHONE HEADSET" 
am$jrb_desc[am$description %in% c("MAGNECTIC STRIPE READER")] <- "MAGNECTIC STRIP READER"
am$jrb_desc[am$description %in% c("MICRO VIEWER")] <- "MICROVIEWER"
am$jrb_desc[am$description %in% c("MOBILE COMPUTER")] <- "LAPTOP"
am$jrb_desc[am$description %in% c("MONITOR 13''")] <- "MONITOR 13\""
am$jrb_desc[am$description %in% c("MONITOR 20'' PL2010M-BK" )] <- "MONITOR 20\""
am$jrb_desc[am$description %in% c("MONITOR 21'", "MONITOR 21\" LCD")] <- "MONITOR 21\""
am$jrb_desc[am$description %in% c("SHERDDER")] <- "SHREDDER"
am$jrb_desc[am$description %in% c("SIGNATURE CAPTURE")] <- "SIGNATURE PAD"
am$jrb_desc[am$description %in% c("SPEAKER SYSTEM / AMP")] <- "SPEAKER"
am$jrb_desc[am$description %in% c("STAPLER ELECTRIC","STAPLER, HEAVY DUTY")] <- "STAPLER"
am$jrb_desc[am$description %in% c("TEN KEYPAD")] <- "TEN KEY PAD"
am$jrb_desc[am$description %in% c("TELEVISION 27''")] <- "TELEVISION 27\"" 
am$jrb_desc[am$description %in% c("TV 20\"")] <- "TELEVISION 20\""
am$jrb_desc[am$description %in% c("TV 50\" LED")] <- "TELEVISION 50\""
am$jrb_desc[am$description %in% c("TV 42\"")] <- "TELEVISION 42\""
am$jrb_desc[am$description %in% c("VIDEO CAMERA ADAPTOR")] <-"VIDEO CAMERA ADAPTER"
am$jrb_desc[am$description %in% c("VIDEO CAMERA W LENS", "VIDEO CAMERA  W LENS")] <- "VIDEO CAMERA W/ LENS"

################################################################################
# Following the meeting on August 7th
################################################################################

# focus on am_duplic and hava datasets
# create item and assignment match keys
# we have 5 different groups, based of items being duplicated on 5 different sets of criteria
# identify duplicated ITEMS (defined by inventory, serial, description bucket)
# identify cases of repeated inventory number, but with different serial and/or description
# consider the ASSIGNMENT (manager + division)
am <- am %>% mutate(item_key = paste(inventory, serial, jrb_desc, sep = " ")) %>% 
  mutate(assign_key = paste(division, manager, sep = " ")) %>%
  mutate(source = "AM") %>% 
  mutate(true_key = paste(item_key, assign_key, sep = " ")) %>% 
  mutate(m1 = paste(inventory, serial, sep = " ")) %>%
  mutate(m2 = paste(inventory, jrb_desc, sep = " "))

hava <- hava %>% mutate(item_key = paste(inventory, serial, jrb_desc, sep = " ")) %>% 
  mutate(assign_key = paste(division, manager, sep = " ")) %>% 
  mutate(source = "HAVA") %>% 
  mutate(true_key = paste(item_key, assign_key, sep = " ")) %>% 
  mutate(m1 = paste(inventory, serial, sep = " "))  %>%
  mutate(m2 = paste(inventory, jrb_desc, sep = " "))

# these are some of the different match keys
# true_key is a truly repeated item
# m1 and m2 are used to parse through repeated inventory numbers that differ on other things 

#####################

am_unique_true_key <- am %>% group_by(true_key) %>% summarise(count = n()) %>% filter(count == 1) %>% arrange(desc(count))
am_duplic_true_key <- am %>% group_by(true_key) %>% summarise(count = n()) %>% filter(count > 1) %>% arrange(desc(count))
# ^ these lists contain all the true keys that are duplicated, and unique

am_true_unique <- am %>% filter(true_key %in% am_unique_true_key$true_key)
am_true_duplic <- am %>% filter(true_key %in% am_duplic_true_key$true_key)
# ^ this code goes back into the dataset, and pulls only the rows that have unique, duplicated true_keys (respectively)

####################
am_unique_item_key <- am %>% group_by(item_key) %>% summarise(count = n()) %>% filter(count == 1) %>% arrange(desc(count))
am_duplic_item_key <- am %>% group_by(item_key) %>% summarise(count = n()) %>% filter(count > 1) %>% arrange(desc(count))
# find the unique, duplicated item_keys

am_item_unique <- am %>% filter(item_key %in% am_unique_item_key$item_key)
am_item_duplic <- am %>% filter(item_key %in% am_duplic_item_key$item_key)
# grab the full data rows for unique, duplicated item_keys

####################

am_duplic_item_assign_keys <- am_duplic %>% group_by(assign_key) %>% summarise(count = n()) %>% filter(count > 1)
am_duplic_item_unique_assign_key <- am_duplic %>% group_by(assign_key) %>% summarise(count = n()) %>% filter(count == 1)
# now looking for repeated and unique assign keys, BUT we are starting with the duplicated inventory number list
# so the output is combo of conditions

am_item_assign_duplics <- am_duplic %>% filter(assign_key %in% am_duplic_item_assign_keys$assign_key)
am_item_duplic_assign_unique <- am_duplic %>% filter(assign_key %in% am_duplic_item_unique_assign_key$assign_key)
# pulling the repeated, unique assign_keys from the list of duplicated inventory numbers

###################

am_duplic_item_key_unique_assign_key <- am %>% group_by(item_key, assign_key) %>% summarise(count1 = n(), count2 = n()) %>% filter(count1 > 1) %>%
  filter(count2 == 1)

am_duplic_item_unique_assign <- am %>% filter(item_key %in% am_duplic_item_key_unique_assign_key$item_key) %>% 
  filter(assign_key %in% am_duplic_item_key_unique_assign_key$assign_key)
# ^ this is scratch work code, no good, but no need to delete
###################
 
m1_dup <- am %>% group_by(m1) %>% summarise(count = n()) %>% filter(count > 1)
m1_uni <- am %>% group_by(m1) %>% summarise(count = n()) %>% filter(count == 1)
am_inv_serial_dup_desc_unique <- am %>% filter(m1 %in% m1_dup$m1) %>% filter(item_key %in% am_item_unique$item_key)

###################

m2_dup <- am %>% group_by(m2) %>% summarise(count = n()) %>% filter(count > 1)
m2_uni <- am %>% group_by(m2) %>% summarise(count = n()) %>% filter(count == 1)
am_inv_desc_dup_serial_unique <- am %>% filter(m2 %in% m2_dup$m2) %>% filter(item_key %in% am_item_unique$item_key)

###################

am_m2_dup <- am %>% filter(m2 %in% m2_dup$m2)
am_m2_uni <- am %>% filter(m2 %in% m2_uni$m2)
am_m1_uni <- am %>% filter(m1 %in% m1_uni$m1)
am_inv_dup_desc_serial_unique <- am_inv_dup %>% filter(m1 %in% am_m1_uni$m1) %>% filter(m2 %in% am_m2_uni$m2)inv_dup_desc_serial_unique <- setdiff(am_inv_dup, rbind(am_inv_serial_dup_desc_unique, am_inv_desc_dup_serial_unique, am_true_duplic, am_duplic_item_unique_assign))

# using combinations of m1 and m2 match key to pull those unique values from the fully duplicated item list
###################
# used View() and some summary statistics to confirm that everything was correct
# repeated this on 6 databases
###################
View(am_true_duplic)
View(am_item_duplic_assign_unique)
View(am_inv_serial_dup_desc_unique)
View(am_inv_desc_dup_serial_unique)
View(am_inv_dup_desc_serial_unique)
################### Now we have all 5 of the lists

