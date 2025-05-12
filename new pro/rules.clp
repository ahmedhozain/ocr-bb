; Medical Tests Knowledge Base in CLIPS

;1) Input facts:
;   (input UserData [Gender] [Age] [AgeLevel])
;   [AgeLevel]: Child / Adult / Elderly
;   (input TestName [unit] [Value] [Ref_Min] [Ref_Max])

;2) Disorders:
;   DIABETES      
;   ANEMIA         
;   HYPOGLYCEMIA   
;   POLYCYTHEMIA    
;   LIVER_DISEASE   
;   KIDNEY_DISEASE  
;   HEART_DISEASE   
;   INFECTION      
;   DEHYDRATION     

;=======================================

(defclass RESULT (is-a USER)
   (slot Dis)
   (slot Level) ;high middle low
   (slot Why)
)

;=======================================
; Templates for test inputs

(deftemplate input
   (slot type)
   (slot gender (default any))
   (slot age (default 0))
   (slot ageLevel (default Adult))
   (slot testName)
   (slot unit)
   (slot value)
   (slot refMin)
   (slot refMax)
)

;=======================================
; Helper function to match test names with synonyms
(deffunction match-test-name (?actual-name ?expected-name)
   (if (eq ?actual-name ?expected-name) then
      (return TRUE)
   else
      (bind ?glucose-synonyms (create$ "Glucose" "GLU" "Blood Sugar" "BS" "FBS" "Sugar" "Glucose" "Blood Sugar" "Fasting Sugar" "Fasting Glucose"))
      (bind ?hemoglobin-synonyms (create$ "Hemoglobin" "HGB" "HB" "Hb" "Hemoglobin" "Blood Hemoglobin" "Hemoglobin" "Blood Hemoglobin"))
      (bind ?rbc-synonyms (create$ "RBC" "Red Blood Cells" "Red Blood Count" "Erythrocytes" "Red Blood Cells" "Red Blood Cells" "Red Blood Cell Count"))
      (bind ?wbc-synonyms (create$ "WBC" "White Blood Cells" "White Blood Count" "Leukocytes" "White Blood Cells" "White Blood Cells" "White Blood Cell Count"))
      (bind ?platelets-synonyms (create$ "Platelets" "PLT" "Thrombocytes" "Blood Platelets" "Blood Platelets" "Platelet Count"))
      (bind ?hct-synonyms (create$ "HCT" "Hematocrit" "PCV" "Hematocrit" "Packed Cell Volume" "Blood Cell Volume" "Hematocrit"))
      (bind ?mcv-synonyms (create$ "MCV" "Mean Corpuscular Volume" "Mean Cell Volume" "Mean Cell Volume" "Mean Corpuscular Volume"))
      (bind ?mch-synonyms (create$ "MCH" "Mean Corpuscular Hemoglobin" "Mean Corpuscular Hemoglobin" "Mean Corpuscular Hemoglobin" "Mean Corpuscular Hemoglobin"))
      (bind ?mchc-synonyms (create$ "MCHC" "Mean Corpuscular Hemoglobin Concentration" "Mean Corpuscular Hemoglobin Concentration" "Mean Corpuscular Hemoglobin Concentration"))
      (bind ?rdw-synonyms (create$ "RDW" "Red Cell Distribution Width" "Red Cell Distribution Width" "Red Cell Distribution Width"))
      (bind ?neutrophils-synonyms (create$ "Neutrophils" "Neut" "Neutro" "Polys" "Segs" "Neutrophils" "Neutrophils" "Granulocytes" "Segmented Neutrophils"))
      (bind ?lymphocytes-synonyms (create$ "Lymphocytes" "Lymphs" "Lym" "Lymphocytes" "Lymphocytes" "Lymphocytes"))
      (bind ?monocytes-synonyms (create$ "Monocytes" "Mono" "Monocytes" "Monocytes" "Monocytes"))
      (bind ?eosinophils-synonyms (create$ "Eosinophils" "Eos" "Eosinophils" "Eosinophils" "Eosinophils"))
      (bind ?basophils-synonyms (create$ "Basophils" "Baso" "Basophils" "Basophils" "Basophils"))
      (bind ?alt-synonyms (create$ "ALT" "SGPT" "Alanine Aminotransferase" "Alanine Aminotransferase" "Liver Enzyme" "ALT Enzyme"))
      (bind ?ast-synonyms (create$ "AST" "SGOT" "Aspartate Aminotransferase" "Aspartate Aminotransferase" "AST Enzyme"))
      (bind ?creatinine-synonyms (create$ "Creatinine" "Cr" "Creatinine" "Creatinine" "Creatinine"))
      (bind ?urea-synonyms (create$ "Urea" "BUN" "Blood Urea Nitrogen" "Urea" "Urea Nitrogen" "Blood Urea"))
      (bind ?cholesterol-synonyms (create$ "Cholesterol" "Total Cholesterol" "TC" "Cholesterol" "Total Cholesterol" "Cholesterol"))
      (bind ?triglycerides-synonyms (create$ "Triglycerides" "TG" "Triglycerides" "Triglycerides" "Triglycerides"))
      (bind ?hdl-synonyms (create$ "HDL" "HDL-C" "High Density Lipoprotein" "High Density Lipoprotein" "Good Cholesterol"))
      (bind ?ldl-synonyms (create$ "LDL" "LDL-C" "Low Density Lipoprotein" "Low Density Lipoprotein" "Bad Cholesterol"))
      (bind ?potassium-synonyms (create$ "Potassium" "K" "K+" "Potassium" "Potassium"))
      (bind ?sodium-synonyms (create$ "Sodium" "Na" "Na+" "Sodium" "Sodium"))
      (bind ?mpv-synonyms (create$ "MPV" "Mean Platelet Volume" "Mean Platelet Volume" "Mean Platelet Volume"))
      (bind ?calcium-synonyms (create$ "Calcium" "Ca" "Ca++" "Calcium" "Calcium"))
      (bind ?phosphorus-synonyms (create$ "Phosphorus" "P" "Phosphorus" "Phosphorus"))
      (bind ?magnesium-synonyms (create$ "Magnesium" "Mg" "Magnesium" "Magnesium"))
      (bind ?iron-synonyms (create$ "Iron" "Fe" "Iron" "Iron"))
      (bind ?ferritin-synonyms (create$ "Ferritin" "Ferritin" "Ferritin"))
      (bind ?tibc-synonyms (create$ "TIBC" "Total Iron Binding Capacity" "Total Iron Binding Capacity"))
      (bind ?transferrin-synonyms (create$ "Transferrin" "Transferrin" "Transferrin"))
      (bind ?a1c-synonyms (create$ "A1C" "HbA1c" "Glycated Hemoglobin" "Glycated Hemoglobin" "Glycosylated Hemoglobin"))
      (bind ?tsh-synonyms (create$ "TSH" "Thyroid Stimulating Hormone" "Thyroid Stimulating Hormone"))
      (bind ?t4-synonyms (create$ "T4" "Thyroxine" "Free T4" "FT4" "Thyroxine" "Free T4"))
      (bind ?t3-synonyms (create$ "T3" "Triiodothyronine" "Free T3" "FT3" "Triiodothyronine" "Free T3"))
      
      ; Check if actual name is in the appropriate synonym list based on expected name
      (if (eq ?expected-name "Glucose") then
         (return (member$ ?actual-name ?glucose-synonyms))
      else (if (eq ?expected-name "Hemoglobin") then
         (return (member$ ?actual-name ?hemoglobin-synonyms))
      else (if (eq ?expected-name "RBC") then
         (return (member$ ?actual-name ?rbc-synonyms))
      else (if (eq ?expected-name "WBC") then
         (return (member$ ?actual-name ?wbc-synonyms))
      else (if (eq ?expected-name "Platelets") then
         (return (member$ ?actual-name ?platelets-synonyms))
      else (if (eq ?expected-name "HCT") then
         (return (member$ ?actual-name ?hct-synonyms))
      else (if (eq ?expected-name "MCV") then
         (return (member$ ?actual-name ?mcv-synonyms))
      else (if (eq ?expected-name "MCH") then
         (return (member$ ?actual-name ?mch-synonyms))
      else (if (eq ?expected-name "MCHC") then
         (return (member$ ?actual-name ?mchc-synonyms))
      else (if (eq ?expected-name "RDW") then
         (return (member$ ?actual-name ?rdw-synonyms))
      else (if (eq ?expected-name "Neutrophils") then
         (return (member$ ?actual-name ?neutrophils-synonyms))
      else (if (eq ?expected-name "Lymphocytes") then
         (return (member$ ?actual-name ?lymphocytes-synonyms))
      else (if (eq ?expected-name "Monocytes") then
         (return (member$ ?actual-name ?monocytes-synonyms))
      else (if (eq ?expected-name "Eosinophils") then
         (return (member$ ?actual-name ?eosinophils-synonyms))
      else (if (eq ?expected-name "Basophils") then
         (return (member$ ?actual-name ?basophils-synonyms))
      else (if (eq ?expected-name "ALT") then
         (return (member$ ?actual-name ?alt-synonyms))
      else (if (eq ?expected-name "AST") then
         (return (member$ ?actual-name ?ast-synonyms))
      else (if (eq ?expected-name "Creatinine") then
         (return (member$ ?actual-name ?creatinine-synonyms))
      else (if (eq ?expected-name "Urea") then
         (return (member$ ?actual-name ?urea-synonyms))
      else (if (eq ?expected-name "Cholesterol") then
         (return (member$ ?actual-name ?cholesterol-synonyms))
      else (if (eq ?expected-name "Triglycerides") then
         (return (member$ ?actual-name ?triglycerides-synonyms))
      else (if (eq ?expected-name "HDL") then
         (return (member$ ?actual-name ?hdl-synonyms))
      else (if (eq ?expected-name "LDL") then
         (return (member$ ?actual-name ?ldl-synonyms))
      else (if (eq ?expected-name "Potassium") then
         (return (member$ ?actual-name ?potassium-synonyms))
      else (if (eq ?expected-name "Sodium") then
         (return (member$ ?actual-name ?sodium-synonyms))
      else (if (eq ?expected-name "MPV") then
         (return (member$ ?actual-name ?mpv-synonyms))
      else (if (eq ?expected-name "Calcium") then
         (return (member$ ?actual-name ?calcium-synonyms))
      else (if (eq ?expected-name "Phosphorus") then
         (return (member$ ?actual-name ?phosphorus-synonyms))
      else (if (eq ?expected-name "Magnesium") then
         (return (member$ ?actual-name ?magnesium-synonyms))
      else (if (eq ?expected-name "Iron") then
         (return (member$ ?actual-name ?iron-synonyms))
      else (if (eq ?expected-name "Ferritin") then
         (return (member$ ?actual-name ?ferritin-synonyms))
      else (if (eq ?expected-name "TIBC") then
         (return (member$ ?actual-name ?tibc-synonyms))
      else (if (eq ?expected-name "Transferrin") then
         (return (member$ ?actual-name ?transferrin-synonyms))
      else (if (eq ?expected-name "A1C") then
         (return (member$ ?actual-name ?a1c-synonyms))
      else (if (eq ?expected-name "TSH") then
         (return (member$ ?actual-name ?tsh-synonyms))
      else (if (eq ?expected-name "T4") then
         (return (member$ ?actual-name ?t4-synonyms))
      else (if (eq ?expected-name "T3") then
         (return (member$ ?actual-name ?t3-synonyms))
      else
         (return FALSE)
      )))))))))))))))))))))))))))))))))))))))
   )
)

; Helper function to match units
(deffunction match-unit (?actual-unit ?expected-unit)
   (if (eq ?actual-unit ?expected-unit) then
      (return TRUE)
   else
      (bind ?mg-dl-synonyms (create$ "mg/dL" "mg/dl" "mg/dl" "mg/dl" "mg/100ml" "mg/100ml"))
      (bind ?g-dl-synonyms (create$ "g/dL" "g/dl" "g/dl" "g/dl" "g/100ml" "g/100ml"))
      (bind ?mmol-l-synonyms (create$ "mmol/L" "mmol/l" "mmol/l" "mmol/l"))
      (bind ?umol-l-synonyms (create$ "umol/L" "µmol/L" "umol/l" "umol/l" "umol/l"))
      (bind ?wbc-unit-synonyms (create$ "x10^3/µL" "10^3/uL" "K/uL" "K/uL" "K/uL" "10^9/L"))
      (bind ?rbc-unit-synonyms (create$ "x10^6/µL" "10^6/uL" "M/uL" "M/uL" "M/uL" "10^12/L"))
      (bind ?percent-synonyms (create$ "%" "?" "percent" "percent"))
      (bind ?u-l-synonyms (create$ "U/L" "u/l" "U/L" "U/L"))
      (bind ?fl-synonyms (create$ "fL" "fl" "fL" "fL"))
      (bind ?pg-synonyms (create$ "pg" "pg" "pg"))
      
      ; Check if actual unit is in the appropriate synonym list based on expected unit
      (if (eq ?expected-unit "mg/dL") then
         (return (member$ ?actual-unit ?mg-dl-synonyms))
      else (if (eq ?expected-unit "g/dL") then
         (return (member$ ?actual-unit ?g-dl-synonyms))
      else (if (eq ?expected-unit "mmol/L") then
         (return (member$ ?actual-unit ?mmol-l-synonyms))
      else (if (eq ?expected-unit "umol/L") then
         (return (member$ ?actual-unit ?umol-l-synonyms))
      else (if (eq ?expected-unit "x10^3/µL") then
         (return (member$ ?actual-unit ?wbc-unit-synonyms))
      else (if (eq ?expected-unit "x10^6/µL") then
         (return (member$ ?actual-unit ?rbc-unit-synonyms))
      else (if (eq ?expected-unit "%") then
         (return (member$ ?actual-unit ?percent-synonyms))
      else (if (eq ?expected-unit "U/L") then
         (return (member$ ?actual-unit ?u-l-synonyms))
      else (if (eq ?expected-unit "fL") then
         (return (member$ ?actual-unit ?fl-synonyms))
      else (if (eq ?expected-unit "pg") then
         (return (member$ ?actual-unit ?pg-synonyms))
      else
         (return FALSE)
      ))))))))))
   )
)

;=======================================
; Blood Tests Rules

; Glucose Rules
(defrule Glucose_High_Adult
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val) (refMax ?max))
   (test (match-test-name ?name "Glucose"))
   (test (match-unit ?unit "mg/dL"))
   (test (> ?val ?max))
   (test (> ?val 126))
=>
   (make-instance of RESULT (Dis DIABETES) (Level high) 
                 (Why (str-cat "High glucose level (" ?val " " ?unit "). Values above 126 " ?unit " indicate diabetes.")))
)

(defrule Glucose_Low
   (input (type UserData) (gender ?gender) (ageLevel ?ageLevel))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val) (refMin ?min))
   (test (match-test-name ?name "Glucose"))
   (test (match-unit ?unit "mg/dL"))
   (test (< ?val ?min))
   (test (< ?val 70))
=>
   (make-instance of RESULT (Dis HYPOGLYCEMIA) (Level low) 
                 (Why (str-cat "Low glucose level (" ?val " " ?unit "). Values below 70 " ?unit " indicate hypoglycemia.")))
)

; Lymphocytes Rules
(defrule Lymphocytes_Low
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Lymphocytes"))
   (test (match-unit ?unit "%"))
   (test (< ?val 20))
=>
   ```clips
   (make-instance of RESULT (Dis LYMPHOPENIA) (Level low) 
                 (Why (str-cat "Low lymphocyte percentage (" ?val " " ?unit "). Values below 20" ?unit " may indicate immune suppression or acute viral infection.")))
)

(defrule Lymphocytes_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Lymphocytes"))
   (test (match-unit ?unit "%"))
   (test (> ?val 40))
=>
   (make-instance of RESULT (Dis LYMPHOCYTOSIS) (Level high) 
                 (Why (str-cat "High lymphocyte percentage (" ?val " " ?unit "). Values above 40" ?unit " may indicate chronic viral infection or autoimmune disorders.")))
)

(defrule Lymphocytes_Absolute_Low
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (or (match-test-name ?name "Absolute Lymphocytes") (match-test-name ?name "Lymphocytes Absolute")))
   (test (match-unit ?unit "x10^3/µL"))
   (test (< ?val 1.0))
=>
   (make-instance of RESULT (Dis LYMPHOPENIA) (Level low) 
                 (Why (str-cat "Low absolute lymphocyte count (" ?val " " ?unit "). Values below 1.0 " ?unit " indicate lymphopenia.")))
)

(defrule Lymphocytes_Absolute_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (or (match-test-name ?name "Absolute Lymphocytes") (match-test-name ?name "Lymphocytes Absolute")))
   (test (match-unit ?unit "x10^3/µL"))
   (test (> ?val 4.0))
=>
   (make-instance of RESULT (Dis LYMPHOCYTOSIS) (Level high) 
                 (Why (str-cat "High absolute lymphocyte count (" ?val " " ?unit "). Values above 4.0 " ?unit " indicate lymphocytosis.")))
)

; Monocytes Rules
(defrule Monocytes_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Monocytes"))
   (test (match-unit ?unit "%"))
   (test (> ?val 10))
=>
   (make-instance of RESULT (Dis MONOCYTOSIS) (Level high) 
                 (Why (str-cat "High monocyte percentage (" ?val " " ?unit "). Values above 10" ?unit " may indicate chronic inflammation or autoimmune diseases.")))
)

(defrule Monocytes_Absolute_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (or (match-test-name ?name "Absolute Monocytes") (match-test-name ?name "Monocytes Absolute")))
   (test (match-unit ?unit "x10^3/µL"))
   (test (> ?val 0.8))
=>
   (make-instance of RESULT (Dis MONOCYTOSIS) (Level high) 
                 (Why (str-cat "High absolute monocyte count (" ?val " " ?unit "). Values above 0.8 " ?unit " indicate monocytosis.")))
)

; Eosinophils Rules
(defrule Eosinophils_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Eosinophils"))
   (test (match-unit ?unit "%"))
   (test (> ?val 5))
=>
   (make-instance of RESULT (Dis EOSINOPHILIA) (Level high) 
                 (Why (str-cat "High eosinophil percentage (" ?val " " ?unit "). Values above 5" ?unit " may indicate allergies or parasitic infection.")))
)

(defrule Eosinophils_Absolute_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (or (match-test-name ?name "Absolute Eosinophils") (match-test-name ?name "Eosinophils Absolute")))
   (test (match-unit ?unit "x10^3/µL"))
   (test (> ?val 0.5))
=>
   (make-instance of RESULT (Dis EOSINOPHILIA) (Level high) 
                 (Why (str-cat "High absolute eosinophil count (" ?val " " ?unit "). Values above 0.5 " ?unit " indicate eosinophilia.")))
)

; Basophils Rules
(defrule Basophils_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Basophils"))
   (test (match-unit ?unit "%"))
   (test (> ?val 2))
=>
   (make-instance of RESULT (Dis BASOPHILIA) (Level high) 
                 (Why (str-cat "High basophil percentage (" ?val " " ?unit "). Values above 2" ?unit " may indicate allergic reactions or bone marrow disorders.")))
)

; Liver Function Tests

; ALT Rules
(defrule ALT_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "ALT"))
   (test (match-unit ?unit "U/L"))
   (test (> ?val 40))
=>
   (make-instance of RESULT (Dis LIVER_DISEASE) (Level high) 
                 (Why (str-cat "High ALT enzyme level (" ?val " " ?unit "). Values above 40 " ?unit " indicate liver cell damage.")))
)

(defrule ALT_Very_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "ALT"))
   (test (match-unit ?unit "U/L"))
   (test (> ?val 200))
=>
   (make-instance of RESULT (Dis ACUTE_LIVER_INJURY) (Level high) 
                 (Why (str-cat "Very high ALT enzyme level (" ?val " " ?unit "). Values above 200 " ?unit " indicate acute liver injury.")))
)

; AST Rules
(defrule AST_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "AST"))
   (test (match-unit ?unit "U/L"))
   (test (> ?val 40))
=>
   (make-instance of RESULT (Dis LIVER_DISEASE) (Level high) 
                 (Why (str-cat "High AST enzyme level (" ?val " " ?unit "). Values above 40 " ?unit " indicate liver or muscle cell damage.")))
)

(defrule AST_Very_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "AST"))
   (test (match-unit ?unit "U/L"))
   (test (> ?val 200))
=>
   (make-instance of RESULT (Dis ACUTE_LIVER_INJURY) (Level high) 
                 (Why (str-cat "Very high AST enzyme level (" ?val " " ?unit "). Values above 200 " ?unit " indicate acute liver injury or muscle damage.")))
)

; Kidney Function Tests

; Creatinine Rules
(defrule Creatinine_High_Male
   (input (type UserData) (gender male) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Creatinine"))
   (test (match-unit ?unit "mg/dL"))
   (test (> ?val 1.2))
=>
   (make-instance of RESULT (Dis KIDNEY_DISEASE) (Level high) 
                 (Why (str-cat "High creatinine level (" ?val " " ?unit "). Values above 1.2 " ?unit " in adult males indicate impaired kidney function.")))
)

(defrule Creatinine_High_Female
   (input (type UserData) (gender female) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Creatinine"))
   (test (match-unit ?unit "mg/dL"))
   (test (> ?val 1.1))
=>
   (make-instance of RESULT (Dis KIDNEY_DISEASE) (Level high) 
                 (Why (str-cat "High creatinine level (" ?val " " ?unit "). Values above 1.1 " ?unit " in adult females indicate impaired kidney function.")))
)

; Urea/BUN Rules
(defrule Urea_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Urea"))
   (test (match-unit ?unit "mg/dL"))
   (test (> ?val 20))
=>
   (make-instance of RESULT (Dis KIDNEY_DISEASE) (Level high) 
                 (Why (str-cat "High urea level (" ?val " " ?unit "). Values above 20 " ?unit " indicate impaired kidney function or dehydration.")))
)

; Lipid Profile

; Cholesterol Rules
(defrule Cholesterol_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Cholesterol"))
   (test (match-unit ?unit "mg/dL"))
   (test (> ?val 200))
=>
   (make-instance of RESULT (Dis HYPERCHOLESTEROLEMIA) (Level high) 
                 (Why (str-cat "High total cholesterol level (" ?val " " ?unit "). Values above 200 " ?unit " increase the risk of cardiovascular disease.")))
)

; Triglycerides Rules
(defrule Triglycerides_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Triglycerides"))
   (test (match-unit ?unit "mg/dL"))
   (test (> ?val 150))
=>
   (make-instance of RESULT (Dis HYPERTRIGLYCERIDEMIA) (Level high) 
                 (Why (str-cat "High triglycerides level (" ?val " " ?unit "). Values above 150 " ?unit " increase the risk of cardiovascular disease.")))
)

; HDL Rules
(defrule HDL_Low_Male
   (input (type UserData) (gender male) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "HDL"))
   (test (match-unit ?unit "mg/dL"))
   (test (< ?val 40))
=>
   (make-instance of RESULT (Dis HEART_DISEASE) (Level low) 
                 (Why (str-cat "Low HDL cholesterol level (" ?val " " ?unit "). Values below 40 " ?unit " in adult males increase the risk of heart disease.")))
)

(defrule HDL_Low_Female
   (input (type UserData) (gender female) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "HDL"))
   (test (match-unit ?unit "mg/dL"))
   (test (< ?val 50))
=>
   (make-instance of RESULT (Dis HEART_DISEASE) (Level low) 
                 (Why (str-cat "Low HDL cholesterol level (" ?val " " ?unit "). Values below 50 " ?unit " in adult females increase the risk of heart disease.")))
)

; LDL Rules
(defrule LDL_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "LDL"))
   (test (match-unit ?unit "mg/dL"))
   (test (> ?val 130))
=>
   (make-instance of RESULT (Dis HEART_DISEASE) (Level high) 
                 (Why (str-cat "High LDL cholesterol level (" ?val " " ?unit "). Values above 130 " ?unit " increase the risk of cardiovascular disease.")))
)

; Electrolytes

; Potassium Rules
(defrule Potassium_Low
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Potassium"))
   (test (match-unit ?unit "mmol/L"))
   (test (< ?val 3.5))
=>
   (make-instance of RESULT (Dis HYPOKALEMIA) (Level low) 
                 (Why (str-cat "Low potassium level (" ?val " " ?unit "). Values below 3.5 " ?unit " may cause muscle weakness and cardiac arrhythmias.")))
)

(defrule Potassium_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Potassium"))
   (test (match-unit ?unit "mmol/L"))
   (test (> ?val 5.0))
=>
   (make-instance of RESULT (Dis HYPERKALEMIA) (Level high) 
                 (Why (str-cat "High potassium level (" ?val " " ?unit "). Values above 5.0 " ?unit " may cause serious cardiac arrhythmias.")))
)

; Sodium Rules
(defrule Sodium_Low
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Sodium"))
   (test (match-unit ?unit "mmol/L"))
   (test (< ?val 135))
=>
   (make-instance of RESULT (Dis HYPONATREMIA) (Level low) 
                 (Why (str-cat "Low sodium level (" ?val " " ?unit "). Values below 135 " ?unit " may cause confusion and seizures.")))
)

(defrule Sodium_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Sodium"))
   (test (match-unit ?unit "mmol/L"))
   (test (> ?val 145))
=>
   (make-instance of RESULT (Dis HYPERNATREMIA) (Level high) 
                 (Why (str-cat "High sodium level (" ?val " " ?unit "). Values above 145 " ?unit " may indicate dehydration.")))
)

; Calcium Rules
(defrule Calcium_Low
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Calcium"))
   (test (match-unit ?unit "mg/dL"))
   (test (< ?val 8.5))
=>
   (make-instance of RESULT (Dis HYPOCALCEMIA) (Level low) 
                 (Why (str-cat "Low calcium level (" ?val " " ?unit "). Values below 8.5 " ?unit " may cause numbness and muscle spasms.")))
)

(defrule Calcium_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Calcium"))
   (test (match-unit ?unit "mg/dL"))
   (test (> ?val 10.5))
=>
   (make-instance of RESULT (Dis HYPERCALCEMIA) (Level high) 
                 (Why (str-cat "High calcium level (" ?val " " ?unit "). Values above 10.5 " ?unit " may cause muscle weakness, constipation, and kidney stones.")))
)

; Iron Studies

; Iron Rules
(defrule Iron_Low
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Iron"))
   (test (match-unit ?unit "µg/dL"))
   (test (< ?val 60))
=>
   (make-instance of RESULT (Dis IRON_DEFICIENCY) (Level low) 
                 (Why (str-cat "Low iron level (" ?val " " ?unit "). Values below 60 " ?unit " may indicate iron deficiency.")))
)

; Ferritin Rules
(defrule Ferritin_Low
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Ferritin"))
   (test (match-unit ?unit "ng/mL"))
   (test (< ?val 30))
=>
   (make-instance of RESULT (Dis IRON_DEFICIENCY) (Level low) 
                 (Why (str-cat "Low ferritin level (" ?val " " ?unit "). Values below 30 " ?unit " indicate depleted iron stores.")))
)

(defrule Ferritin_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Ferritin"))
   (test (match-unit ?unit "ng/mL"))
   (test (> ?val 300))
=>
   (make-instance of RESULT (Dis IRON_OVERLOAD) (Level high) 
                 (Why (str-cat "High ferritin level (" ?val " " ?unit "). Values above 300 " ?unit " may indicate iron overload or chronic inflammation.")))
)

; TIBC Rules
(defrule TIBC_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "TIBC"))
   (test (match-unit ?unit "µg/dL"))
   (test (> ?val 400))
=>
   (make-instance of RESULT (Dis IRON_DEFICIENCY) (Level high) 
                 (Why (str-cat "High total iron binding capacity (" ?val " " ?unit "). Values above 400 " ?unit " indicate iron deficiency.")))
)

; Transferrin Saturation Rules
(defrule Transferrin_Saturation_Low
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Transferrin Saturation"))
   (test (match-unit ?unit "%"))
   (test (< ?val 15))
=>
   (make-instance of RESULT (Dis IRON_DEFICIENCY) (Level low) 
                 (Why (str-cat "Low transferrin saturation (" ?val " " ?unit "). Values below 15" ?unit " indicate iron deficiency.")))
)

(defrule Transferrin_Saturation_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Transferrin Saturation"))
   (test (match-unit ?unit "%"))
   (test (> ?val 50))
=>
   (make-instance of RESULT (Dis IRON_OVERLOAD) (Level high) 
                 (Why (str-cat "High transferrin saturation (" ?val " " ?unit "). Values above 50" ?unit " may indicate iron overload.")))
)

; Glycated Hemoglobin (HbA1c) Rules
(defrule A1C_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "A1C"))
   (test (match-unit ?unit "%"))
   (test (> ?val 6.5))
=>
   (make-instance of RESULT (Dis DIABETES) (Level high) 
                 (Why (str-cat "High glycated hemoglobin level (" ?val " " ?unit "). Values above 6.5" ?unit " diagnose diabetes.")))
)

(defrule A1C_Prediabetes
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "A1C"))
   (test (match-unit ?unit "%"))
   (test (and (>= ?val 5.7) (<= ?val 6.4)))
=>
   ```clips
   (make-instance of RESULT (Dis PREDIABETES) (Level middle) 
                 (Why (str-cat "Borderline glycated hemoglobin level (" ?val " " ?unit "). Values between 5.7 and 6.4" ?unit " indicate prediabetes.")))
)

; Thyroid Function Tests

; TSH Rules
(defrule TSH_Low
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "TSH"))
   (test (match-unit ?unit "mIU/L"))
   (test (< ?val 0.4))
=>
   (make-instance of RESULT (Dis HYPERTHYROIDISM) (Level low) 
                 (Why (str-cat "Low TSH hormone level (" ?val " " ?unit "). Values below 0.4 " ?unit " indicate hyperthyroidism.")))
)

(defrule TSH_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "TSH"))
   (test (match-unit ?unit "mIU/L"))
   (test (> ?val 4.0))
=>
   (make-instance of RESULT (Dis HYPOTHYROIDISM) (Level high) 
                 (Why (str-cat "High TSH hormone level (" ?val " " ?unit "). Values above 4.0 " ?unit " indicate hypothyroidism.")))
)

; Free T4 Rules
(defrule T4_Low
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "T4"))
   (test (match-unit ?unit "ng/dL"))
   (test (< ?val 0.8))
=>
   (make-instance of RESULT (Dis HYPOTHYROIDISM) (Level low) 
                 (Why (str-cat "Low free T4 hormone level (" ?val " " ?unit "). Values below 0.8 " ?unit " indicate hypothyroidism.")))
)

(defrule T4_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "T4"))
   (test (match-unit ?unit "ng/dL"))
   (test (> ?val 1.8))
=>
   (make-instance of RESULT (Dis HYPERTHYROIDISM) (Level high) 
                 (Why (str-cat "High free T4 hormone level (" ?val " " ?unit "). Values above 1.8 " ?unit " indicate hyperthyroidism.")))
)

; Free T3 Rules
(defrule T3_Low
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "T3"))
   (test (match-unit ?unit "pg/mL"))
   (test (< ?val 2.3))
=>
   (make-instance of RESULT (Dis HYPOTHYROIDISM) (Level low) 
                 (Why (str-cat "Low free T3 hormone level (" ?val " " ?unit "). Values below 2.3 " ?unit " indicate hypothyroidism.")))
)

(defrule T3_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "T3"))
   (test (match-unit ?unit "pg/mL"))
   (test (> ?val 4.2))
=>
   (make-instance of RESULT (Dis HYPERTHYROIDISM) (Level high) 
                 (Why (str-cat "High free T3 hormone level (" ?val " " ?unit "). Values above 4.2 " ?unit " indicate hyperthyroidism.")))
)

; Dehydration Indicators

(defrule Hematocrit_Dehydration
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "HCT"))
   (test (match-unit ?unit "%"))
   (test (> ?val 52))
=>
   (make-instance of RESULT (Dis DEHYDRATION) (Level high) 
                 (Why (str-cat "Very high hematocrit percentage (" ?val " " ?unit "). Values above 52" ?unit " may indicate dehydration.")))
)

(defrule BUN_Creatinine_Ratio_High
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name1) (unit ?unit1) (value ?val1))
   (input (type TestData) (testName ?name2) (unit ?unit2) (value ?val2))
   (test (match-test-name ?name1 "Urea"))
   (test (match-test-name ?name2 "Creatinine"))
   (test (match-unit ?unit1 "mg/dL"))
   (test (match-unit ?unit2 "mg/dL"))
   (test (> (/ ?val1 ?val2) 20))
=>
   (make-instance of RESULT (Dis DEHYDRATION) (Level high) 
                 (Why (str-cat "High BUN to creatinine ratio (" (/ ?val1 ?val2) "). Ratio above 20 may indicate dehydration.")))
)

; Child-specific Rules

(defrule Hemoglobin_Low_Child
   (input (type UserData) (gender ?gender) (ageLevel Child))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Hemoglobin"))
   (test (match-unit ?unit "g/dL"))
   (test (< ?val 11.0))
=>
   (make-instance of RESULT (Dis ANEMIA) (Level low) 
                 (Why (str-cat "Low hemoglobin level (" ?val " " ?unit "). Values below 11.0 " ?unit " in children indicate anemia.")))
)

(defrule WBC_High_Child
   (input (type UserData) (gender ?gender) (ageLevel Child))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "WBC"))
   (test (match-unit ?unit "x10^3/µL"))
   (test (> ?val 15.0))
=>
   (make-instance of RESULT (Dis INFECTION) (Level high) 
                 (Why (str-cat "High white blood cell count (" ?val " " ?unit "). Values above 15.0 " ?unit " in children indicate infection or inflammation.")))
)

; Elderly-specific Rules

(defrule Creatinine_High_Elderly
   (input (type UserData) (gender ?gender) (ageLevel Elderly))
   (input (type TestData) (testName ?name) (unit ?unit) (value ?val))
   (test (match-test-name ?name "Creatinine"))
   (test (match-unit ?unit "mg/dL"))
   (test (> ?val 1.3))
=>
   (make-instance of RESULT (Dis KIDNEY_DISEASE) (Level high) 
                 (Why (str-cat "High creatinine level (" ?val " " ?unit "). Values above 1.3 " ?unit " in elderly indicate impaired kidney function, which is common with aging.")))
)

; Multiple Test Patterns

(defrule Anemia_Pattern_Microcytic
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name1) (unit ?unit1) (value ?val1))
   (input (type TestData) (testName ?name2) (unit ?unit2) (value ?val2))
   (input (type TestData) (testName ?name3) (unit ?unit3) (value ?val3))
   (test (match-test-name ?name1 "Hemoglobin"))
   (test (match-test-name ?name2 "MCV"))
   (test (match-test-name ?name3 "Ferritin"))
   (test (match-unit ?unit1 "g/dL"))
   (test (match-unit ?unit2 "fL"))
   (test (match-unit ?unit3 "ng/mL"))
   (test (< ?val1 12.0))
   (test (< ?val2 80))
   (test (< ?val3 30))
=>
   (make-instance of RESULT (Dis IRON_DEFICIENCY_ANEMIA) (Level low) 
                 (Why (str-cat "Microcytic anemia pattern: Low hemoglobin (" ?val1 " " ?unit1 "), low MCV (" ?val2 " " ?unit2 "), and low ferritin (" ?val3 " " ?unit3 "). This pattern indicates iron deficiency anemia.")))
)

(defrule Anemia_Pattern_Macrocytic
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name1) (unit ?unit1) (value ?val1))
   (input (type TestData) (testName ?name2) (unit ?unit2) (value ?val2))
   (test (match-test-name ?name1 "Hemoglobin"))
   (test (match-test-name ?name2 "MCV"))
   (test (match-unit ?unit1 "g/dL"))
   (test (match-unit ?unit2 "fL"))
   (test (< ?val1 12.0))
   (test (> ?val2 100))
=>
   (make-instance of RESULT (Dis MACROCYTIC_ANEMIA) (Level low) 
                 (Why (str-cat "Macrocytic anemia pattern: Low hemoglobin (" ?val1 " " ?unit1 ") and high MCV (" ?val2 " " ?unit2 "). This pattern may indicate vitamin B12 or folate deficiency.")))
)

(defrule Metabolic_Syndrome_Pattern
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name1) (unit ?unit1) (value ?val1))
   (input (type TestData) (testName ?name2) (unit ?unit2) (value ?val2))
   (input (type TestData) (testName ?name3) (unit ?unit3) (value ?val3))
   (test (match-test-name ?name1 "Glucose"))
   (test (match-test-name ?name2 "Triglycerides"))
   (test (match-test-name ?name3 "HDL"))
   (test (match-unit ?unit1 "mg/dL"))
   (test (match-unit ?unit2 "mg/dL"))
   (test (match-unit ?unit3 "mg/dL"))
   (test (> ?val1 100))
   (test (> ?val2 150))
   (test (< ?val3 40))
=>
   (make-instance of RESULT (Dis METABOLIC_SYNDROME) (Level high) 
                 (Why (str-cat "Metabolic syndrome pattern: High glucose (" ?val1 " " ?unit1 "), high triglycerides (" ?val2 " " ?unit2 "), and low HDL cholesterol (" ?val3 " " ?unit3 "). This pattern indicates metabolic syndrome, which increases the risk of heart disease and diabetes.")))
)

(defrule Liver_Disease_Pattern
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name1) (unit ?unit1) (value ?val1))
   (input (type TestData) (testName ?name2) (unit ?unit2) (value ?val2))
   (test (match-test-name ?name1 "ALT"))
   (test (match-test-name ?name2 "AST"))
   (test (match-unit ?unit1 "U/L"))
   (test (match-unit ?unit2 "U/L"))
   (test (> ?val1 40))
   (test (> ?val2 40))
=>
   (make-instance of RESULT (Dis LIVER_DISEASE) (Level high) 
                 (Why (str-cat "Liver disease pattern: High ALT enzyme (" ?val1 " " ?unit1 ") and high AST enzyme (" ?val2 " " ?unit2 "). This pattern indicates liver cell damage.")))
)

; Rules for special cases and combinations

(defrule Kidney_Disease_Pattern
   (input (type UserData) (gender ?gender) (ageLevel Adult))
   (input (type TestData) (testName ?name1) (unit ?unit1) (value ?val1))
   (input (type TestData) (testName ?name2) (unit ?unit2) (value ?val2))
   (test (match-test-name ?name1 "Creatinine"))
   (test (match-test-name ?name2 "Urea"))
   (test (match-unit ?unit1 "mg/dL"))
   (test (match-unit ?unit2 "mg/dL"))
   (test (> ?val1 1.2))
   (test (> ?val2 20))
=>
   (make-instance of RESULT (Dis KIDNEY_DISEASE) (Level high) 
                 (Why (str-cat "Kidney disease pattern: High creatinine (" ?val1 " " ?unit1 ") and high urea (" ?val2 " " ?unit2 "). This pattern indicates impaired kidney function.")))
)

; Helper rules for data conversion

(defrule Convert_Glucose_mmol_to_mg
   ?f <- (input (type TestData) (testName ?name) (unit ?unit) (value ?val) (refMin ?min) (refMax ?max))
   (test (match-test-name ?name "Glucose"))
   (test (match-unit ?unit "mmol/L"))
=>
   (modify ?f (unit "mg/dL") (value (* ?val 18)) (refMin (* ?min 18)) (refMax (* ?max 18)))
)

(defrule Convert_Triglycerides_mmol_to_mg
   ?f <- (input (type TestData) (testName ?name) (unit ?unit) (value ?val) (refMin ?min) (refMax ?max))
   (test (match-test-name ?name "Triglycerides"))
   (test (match-unit ?unit "mmol/L"))
=>
   (modify ?f (unit "mg/dL") (value (* ?val 88.57)) (refMin (* ?min 88.57)) (refMax (* ?max 88.57)))
)

(defrule Convert_HDL_mmol_to_mg
   ?f <- (input (type TestData) (testName ?name) (unit ?unit) (value ?val) (refMin ?min) (refMax ?max))
   (test (match-test-name ?name "HDL"))
   (test (match-unit ?unit "mmol/L"))
=>
   (modify ?f (unit "mg/dL") (value (* ?val 38.67)) (refMin (* ?min 38.67)) (refMax (* ?max 38.67)))
)

(defrule Convert_LDL_mmol_to_mg
   ?f <- (input (type TestData) (testName ?name) (unit ?unit) (value ?val) (refMin ?min) (refMax ?max))
   (test (match-test-name ?name "LDL"))
   (test (match-unit ?unit "mmol/L"))
=>
   (modify ?f (unit "mg/dL") (value (* ?val 38.67)) (refMin (* ?min 38.67)) (refMax (* ?max 38.67)))
)

(defrule Convert_Creatinine_umol_to_mg
   ?f <- (input (type TestData) (testName ?name) (unit ?unit) (value ?val) (refMin ?min) (refMax ?max))
   (test (match-test-name ?name "Creatinine"))
   (test (match-unit ?unit "umol/L"))
=>
   (modify ?f (unit "mg/dL") (value (/ ?val 88.4)) (refMin (/ ?min 88.4)) (refMax (/ ?max 88.4)))
)

(defrule Convert_Urea_mmol_to_mg
   ?f <- (input (type TestData) (testName ?name) (unit ?unit) (value ?val) (refMin ?min) (refMax ?max))
   (test (match-test-name ?name "Urea"))
   (test (match-unit ?unit "mmol/L"))
=>
   (modify ?f (unit "mg/dL") (value (* ?val 2.8)) (refMin (* ?min 2.8)) (refMax (* ?max 2.8)))
)

; Rules for handling multiple test results for the same test

(defrule Average_Multiple_Glucose_Tests
   (declare (salience -10))
   (input (type TestData) (testName ?name1) (unit ?unit1) (value ?val1))
   (input (type TestData) (testName ?name2) (unit ?unit2) (value ?val2))
   (test (match-test-name ?name1 "Glucose"))
   (test (match-test-name ?name2 "Glucose"))
   (test (match-unit ?unit1 "mg/dL"))
   (test (match-unit ?unit2 "mg/dL"))
   (test (neq ?name1 ?name2))
=>
   (assert (input (type TestData) (testName "Glucose Average") (unit "mg/dL") (value (/ (+ ?val1 ?val2) 2))))
)

; Rules for generating summary reports

(defrule Generate_Anemia_Summary
   (declare (salience -100))
   ?r1 <- (object (is-a RESULT) (Dis ANEMIA) (Level ?level1) (Why ?why1))
   ?r2 <- (object (is-a RESULT) (Dis IRON_DEFICIENCY) (Level ?level2) (Why ?why2))
=>
   (make-instance of RESULT (Dis ANEMIA_SUMMARY) (Level ?level1) 
                 (Why (str-cat "Anemia Summary: " ?why1 " " ?why2 " The cause appears to be iron deficiency. It is recommended to consume iron-rich foods such as red meat and spinach, and iron supplements may be needed under medical supervision.")))
)

(defrule Generate_Diabetes_Summary
   (declare (salience -100))
   ?r1 <- (object (is-a RESULT) (Dis DIABETES) (Level ?level1) (Why ?why1))
=>
   (make-instance of RESULT (Dis DIABETES_SUMMARY) (Level ?level1) 
                 (Why (str-cat "Diabetes Summary: " ?why1 " It is recommended to consult a doctor to confirm the diagnosis and start appropriate treatment. Attention should be paid to diet, exercise, and maintaining a healthy weight.")))
)

(defrule Generate_Liver_Disease_Summary
   (declare (salience -100))
   ?r1 <- (object (is-a RESULT) (Dis LIVER_DISEASE) (Level ?level1) (Why ?why1))
=>
   (make-instance of RESULT (Dis LIVER_DISEASE_SUMMARY) (Level ?level1) 
                 (Why (str-cat "Liver Disease Summary: " ?why1 " It is recommended to consult a specialist to determine the cause of elevated liver enzymes and start appropriate treatment. Alcohol and medications that may affect the liver should be avoided.")))
)

(defrule Generate_Kidney_Disease_Summary
   (declare (salience -100))
   ?r1 <- (object (is-a RESULT) (Dis KIDNEY_DISEASE) (Level ?level1) (Why ?why1))
=>
   (make-instance of RESULT (Dis KIDNEY_DISEASE_SUMMARY) (Level ?level1) 
                 (Why (str-cat "Kidney Disease Summary: " ?why1 " It is recommended to consult a specialist for accurate assessment of kidney function. Attention should be paid to controlling blood pressure and blood sugar levels, and avoiding medications that may affect the kidneys.")))
)

(defrule Generate_Heart_Disease_Summary
   (declare (salience -100))
   ?r1 <- (object (is-a RESULT) (Dis HEART_DISEASE) (Level ?level1) (Why ?why1))
=>
   (make-instance of RESULT (Dis HEART_DISEASE_SUMMARY) (Level ?level1) 
                 (Why (str-cat "Heart Disease Summary: " ?why1 " It is recommended to consult a cardiologist to assess cardiovascular risks. Attention should be paid to a healthy diet, regular exercise, and smoking cessation.")))
)

; Rules for age-specific recommendations

(defrule Child_Recommendations
   (declare (salience -110))
   (input (type UserData) (gender ?gender) (ageLevel Child))
   (object (is-a RESULT) (Dis ?dis))
=>
   (make-instance of RESULT (Dis CHILD_RECOMMENDATIONS) (Level middle) 
                 (Why "Recommendations for Children: The child's growth and development should be monitored regularly. Ensure the child receives a balanced diet rich in vitamins and minerals essential for growth. Consult a pediatrician before giving any dietary supplements."))
)

(defrule Elderly_Recommendations
   (declare (salience -110))
   (input (type UserData) (gender ?gender) (ageLevel Elderly))
   (object (is-a RESULT) (Dis ?dis))
=>
   ```clips
   (make-instance of RESULT (Dis ELDERLY_RECOMMENDATIONS) (Level middle) 
                 (Why "Recommendations for Elderly: It is important to have regular check-ups. Consume a balanced diet rich in calcium and vitamin D to maintain bone health. Maintain physical activity appropriate for your age and health condition. Avoid falls and take necessary precautions at home."))
)

; Final summary rule
(defrule Generate_Final_Summary
   (declare (salience -120))
   (input (type UserData) (gender ?gender) (ageLevel ?ageLevel))
=>
   (make-instance of RESULT (Dis FINAL_SUMMARY) (Level middle) 
                 (Why "Important Note: These results are based solely on laboratory values analysis and do not constitute a final medical diagnosis. Please consult a specialist to interpret the results in the context of your complete health status, medical history, and clinical symptoms."))
)

;=======================================
; Examples of how to use the system:

; Example 1: Diabetes case
;(reset)
;(assert (input (type UserData) (gender male) (age 45) (ageLevel Adult)))
;(assert (input (type TestData) (testName "Glucose") (unit "mg/dL") (value 140) (refMin 70) (refMax 126)))
;(assert (input (type TestData) (testName "Hemoglobin") (unit "g/dL") (value 14.5) (refMin 13.0) (refMax 17.0)))
;(assert (input (type TestData) (testName "A1C") (unit "%") (value 7.2) (refMin 4.0) (refMax 5.6)))
;(run)

; Example 2: Anemia case
;(reset)
;(assert (input (type UserData) (gender female) (age 35) (ageLevel Adult)))
;(assert (input (type TestData) (testName "Hemoglobin") (unit "g/dL") (value 10.5) (refMin 12.0) (refMax 16.0)))
;(assert (input (type TestData) (testName "MCV") (unit "fL") (value 75) (refMin 80) (refMax 100)))
;(assert (input (type TestData) (testName "Ferritin") (unit "ng/mL") (value 15) (refMin 30) (refMax 200)))
;(assert (input (type TestData) (testName "Iron") (unit "µg/dL") (value 45) (refMin 60) (refMax 170)))
;(run)

; Example 3: Liver disease case
;(reset)
;(assert (input (type UserData) (gender male) (age 50) (ageLevel Adult)))
;(assert (input (type TestData) (testName "ALT") (unit "U/L") (value 95) (refMin 7) (refMax 40)))
;(assert (input (type TestData) (testName "AST") (unit "U/L") (value 85) (refMin 10) (refMax 40)))
;(assert (input (type TestData) (testName "Total Bilirubin") (unit "mg/dL") (value 1.8) (refMin 0.1) (refMax 1.2)))
;(run)

; Example 4: Kidney disease case
;(reset)
;(assert (input (type UserData) (gender female) (age 65) (ageLevel Elderly)))
;(assert (input (type TestData) (testName "Creatinine") (unit "mg/dL") (value 1.8) (refMin 0.6) (refMax 1.1)))
;(assert (input (type TestData) (testName "Urea") (unit "mg/dL") (value 45) (refMin 8) (refMax 20)))
;(assert (input (type TestData) (testName "Potassium") (unit "mmol/L") (value 5.2) (refMin 3.5) (refMax 5.0)))
;(run)

; Example 5: Infection case
;(reset)
;(assert (input (type UserData) (gender male) (age 30) (ageLevel Adult)))
;(assert (input (type TestData) (testName "WBC") (unit "x10^3/µL") (value 15.5) (refMin 4.0) (refMax 11.0)))
;(assert (input (type TestData) (testName "Neutrophils") (unit "%") (value 80) (refMin 40) (refMax 75)))
;(assert (input (type TestData) (testName "C-Reactive Protein") (unit "mg/L") (value 45) (refMin 0) (refMax 5)))
;(run)

; Example 6: Thyroid disorder case
;(reset)
;(assert (input (type UserData) (gender female) (age 42) (ageLevel Adult)))
;(assert (input (type TestData) (testName "TSH") (unit "mIU/L") (value 0.1) (refMin 0.4) (refMax 4.0)))
;(assert (input (type TestData) (testName "T4") (unit "ng/dL") (value 2.3) (refMin 0.8) (refMax 1.8)))
;(assert (input (type TestData) (testName "T3") (unit "pg/mL") (value 5.1) (refMin 2.3) (refMax 4.2)))
;(run)

; Example 7: Lipid profile abnormalities
;(reset)
;(assert (input (type UserData) (gender male) (age 55) (ageLevel Adult)))
;(assert (input (type TestData) (testName "Cholesterol") (unit "mg/dL") (value 245) (refMin 0) (refMax 200)))
;(assert (input (type TestData) (testName "Triglycerides") (unit "mg/dL") (value 220) (refMin 0) (refMax 150)))
;(assert (input (type TestData) (testName "HDL") (unit "mg/dL") (value 35) (refMin 40) (refMax 60)))
;(assert (input (type TestData) (testName "LDL") (unit "mg/dL") (value 165) (refMin 0) (refMax 130)))
;(run)

; Example 8: Dehydration case
;(reset)
;(assert (input (type UserData) (gender female) (age 75) (ageLevel Elderly)))
;(assert (input (type TestData) (testName "Sodium") (unit "mmol/L") (value 148) (refMin 135) (refMax 145)))
;(assert (input (type TestData) (testName "HCT") (unit "%") (value 52) (refMin 36) (refMax 47)))
;(assert (input (type TestData) (testName "Urea") (unit "mg/dL") (value 35) (refMin 8) (refMax 20)))
;(assert (input (type TestData) (testName "Creatinine") (unit "mg/dL") (value 1.4) (refMin 0.6) (refMax 1.1)))
;(run)

; Example 9: Using English test names
;(reset)
;(assert (input (type UserData) (gender male) (age 60) (ageLevel Adult)))
;(assert (input (type TestData) (testName "Glucose") (unit "mg/dL") (value 190) (refMin 70) (refMax 100)))
;(assert (input (type TestData) (testName "Hemoglobin") (unit "g/dL") (value 13.5) (refMin 13.0) (refMax 17.0)))
;(assert (input (type TestData) (testName "Creatinine") (unit "mg/dL") (value 1.5) (refMin 0.7) (refMax 1.2)))
;(run)

; Example 10: Child case
;(reset)
;(assert (input (type UserData) (gender male) (age 8) (ageLevel Child)))
;(assert (input (type TestData) (testName "Hemoglobin") (unit "g/dL") (value 10.5) (refMin 11.0) (refMax 14.0)))
;(assert (input (type TestData) (testName "WBC") (unit "x10^3/µL") (value 16.0) (refMin 5.0) (refMax 14.5)))
;(assert (input (type TestData) (testName "Platelets") (unit "x10^3/µL") (value 450) (refMin 150) (refMax 450)))
;(run)

; Function to display results
(deffunction display-results ()
   (do-for-all-instances ((?r RESULT)) TRUE
      (printout t "Disorder: " ?r:Dis " (Level: " ?r:Level ")" crlf
                "Reason: " ?r:Why crlf crlf))
)

; Function to run a specific example
(deffunction run-example (?example-number)
   (reset)
   (if (= ?example-number 1) then
      ; Example 1: Diabetes case
      (assert (input (type UserData) (gender male) (age 45) (ageLevel Adult)))
      (assert (input (type TestData) (testName "Glucose") (unit "mg/dL") (value 140) (refMin 70) (refMax 126)))
      (assert (input (type TestData) (testName "Hemoglobin") (unit "g/dL") (value 14.5) (refMin 13.0) (refMax 17.0)))
      (assert (input (type TestData) (testName "A1C") (unit "%") (value 7.2) (refMin 4.0) (refMax 5.6)))
      (printout t "Running Example 1: Diabetes case" crlf)
   else (if (= ?example-number 2) then
      ; Example 2: Anemia case
      (assert (input (type UserData) (gender female) (age 35) (ageLevel Adult)))
      (assert (input (type TestData) (testName "Hemoglobin") (unit "g/dL") (value 10.5) (refMin 12.0) (refMax 16.0)))
      (assert (input (type TestData) (testName "MCV") (unit "fL") (value 75) (refMin 80) (refMax 100)))
      (assert (input (type TestData) (testName "Ferritin") (unit "ng/mL") (value 15) (refMin 30) (refMax 200)))
      (assert (input (type TestData) (testName "Iron") (unit "µg/dL") (value 45) (refMin 60) (refMax 170)))
      (printout t "Running Example 2: Anemia case" crlf)
   else (if (= ?example-number 3) then
      ; Example 3: Liver disease case
      (assert (input (type UserData) (gender male) (age 50) (ageLevel Adult)))
      (assert (input (type TestData) (testName "ALT") (unit "U/L") (value 95) (refMin 7) (refMax 40)))
      (assert (input (type TestData) (testName "AST") (unit "U/L") (value 85) (refMin 10) (refMax 40)))
      (assert (input (type TestData) (testName "Total Bilirubin") (unit "mg/dL") (value 1.8) (refMin 0.1) (refMax 1.2)))
      (printout t "Running Example 3: Liver disease case" crlf)
   else (if (= ?example-number 4) then
      ; Example 4: Kidney disease case
      (assert (input (type UserData) (gender female) (age 65) (ageLevel Elderly)))
      (assert (input (type TestData) (testName "Creatinine") (unit "mg/dL") (value 1.8) (refMin 0.6) (refMax 1.1)))
      (assert (input (type TestData) (testName "Urea") (unit "mg/dL") (value 45) (refMin 8) (refMax 20)))
      (assert (input (type TestData) (testName "Potassium") (unit "mmol/L") (value 5.2) (refMin 3.5) (refMax 5.0)))
      (printout t "Running Example 4: Kidney disease case" crlf)
   else (if (= ?example-number 5) then
      ; Example 5: Infection case
      (assert (input (type UserData) (gender male) (age 30) (ageLevel Adult)))
      (assert (input (type TestData) (testName "WBC") (unit "x10^3/µL") (value 15.5) (refMin 4.0) (refMax 11.0)))
      (assert (input (type TestData) (testName "Neutrophils") (unit "%") (value 80) (refMin 40) (refMax 75)))
      (assert (input (type TestData) (testName "C-Reactive Protein") (unit "mg/L") (value 45) (refMin 0) (refMax 5)))
      (printout t "Running Example 5: Infection case" crlf)
   else (if (= ?example-number 6) then
      ; Example 6: Thyroid disorder case
      (assert (input (type UserData) (gender female) (age 42) (ageLevel Adult)))
      (assert (input (type TestData) (testName "TSH") (unit "mIU/L") (value 0.1) (refMin 0.4) (refMax 4.0)))
      (assert (input (type TestData) (testName "T4") (unit "ng/dL") (value 2.3) (refMin 0.8) (refMax 1.8)))
      (assert (input (type TestData) (testName "T3") (unit "pg/mL") (value 5.1) (refMin 2.3) (refMax 4.2)))
      (printout t "Running Example 6: Thyroid disorder case" crlf)
   else (if (= ?example-number 7) then
      ; Example 7: Lipid profile abnormalities
      (assert (input (type UserData) (gender male) (age 55) (ageLevel Adult)))
      (assert (input (type TestData) (testName "Cholesterol") (unit "mg/dL") (value 245) (refMin 0) (refMax 200)))
      (assert (input (type TestData) (testName "Triglycerides") (unit "mg/dL") (value 220) (refMin 0) (refMax 150)))
      (assert (input (type TestData) (testName "HDL") (unit "mg/dL") (value 35) (refMin 40) (refMax 60)))
      (assert (input (type TestData) (testName "LDL") (unit "mg/dL") (value 165) (refMin 0) (refMax 130)))
      (printout t "Running Example 7: Lipid profile abnormalities" crlf)
   else (if (= ?example-number 8) then
      ; Example 8: Dehydration case
      (assert (input (type UserData) (gender female) (age 75) (ageLevel Elderly)))
      (assert (input (type TestData) (testName "Sodium") (unit "mmol/L") (value 148) (refMin 135) (refMax 145)))
      (assert (input (type TestData) (testName "HCT") (unit "%") (value 52) (refMin 36) (refMax 47)))
      (assert (input (type TestData) (testName "Urea") (unit "mg/dL") (value 35) (refMin 8) (refMax 20)))
      (assert (input (type TestData) (testName "Creatinine") (unit "mg/dL") (value 1.4) (refMin 0.6) (refMax 1.1)))
      (printout t "Running Example 8: Dehydration case" crlf)
   else (if (= ?example-number 9) then
      ; Example 9: Using English test names
      (assert (input (type UserData) (gender male) (age 60) (ageLevel Adult)))
      (assert (input (type TestData) (testName "Glucose") (unit "mg/dL") (value 190) (refMin 70) (refMax 100)))
      (assert (input (type TestData) (testName "Hemoglobin") (unit "g/dL") (value 13.5) (refMin 13.0) (refMax 17.0)))
      (assert (input (type TestData) (testName "Creatinine") (unit "mg/dL") (value 1.5) (refMin 0.7) (refMax 1.2)))
      (printout t "Running Example 9: High glucose case" crlf)
   else (if (= ?example-number 10) then
      ; Example 10: Child case
      (assert (input (type UserData) (gender male) (age 8) (ageLevel Child)))
      (assert (input (type TestData) (testName "Hemoglobin") (unit "g/dL") (value 10.5) (refMin 11.0) (refMax 14.0)))
      (assert (input (type TestData) (testName "WBC") (unit "x10^3/µL") (value 16.0) (refMin 5.0) (refMax 14.5)))
      (assert (input (type TestData) (testName "Platelets") (unit "x10^3/µL") (value 450) (refMin 150) (refMax 450)))
      (printout t "Running Example 10: Child case" crlf)
   else
      (printout t "Invalid example number. Please choose a number between 1 and 10." crlf)
      (return)
   ))))))))))
   
   (run)
   (printout t crlf "=== RESULTS ===" crlf crlf)
   (display-results)
)

; Function to create a custom test case
(deffunction custom-test-case ()
   (reset)
   (printout t "Enter patient information:" crlf)
   
   (printout t "Gender (male/female): ")
   (bind ?gender (read))
   
   (printout t "Age: ")
   (bind ?age (read))
   
   (bind ?ageLevel "Adult")
   (if (< ?age 18) then
      (bind ?ageLevel "Child")
   else (if (> ?age 65) then
      (bind ?ageLevel "Elderly")
   ))
   
   (assert (input (type UserData) (gender ?gender) (age ?age) (ageLevel ?ageLevel)))
   
   (printout t crlf "Enter test results (enter 'done' for test name when finished):" crlf)
   (bind ?done FALSE)
   
   (while (not ?done)
      (printout t crlf "Test name: ")
      (bind ?testName (readline))
      
      (if (eq ?testName "done") then
         (bind ?done TRUE)
      else
         (printout t "Unit: ")
         (bind ?unit (readline))
         
         (printout t "Value: ")
         (bind ?value (read))
         
         (printout t "Reference minimum: ")
         (bind ?refMin (read))
         
         (printout t "Reference maximum: ")
         (bind ?refMax (read))
         
         (assert (input (type TestData) (testName ?testName) (unit ?unit) 
                        (value ?value) (refMin ?refMin) (refMax ?refMax)))
      )
   )
   
   (printout t crlf "Processing test results..." crlf)
   (run)
   (printout t crlf "=== RESULTS ===" crlf crlf)
   (display-results)
)

; Main menu function
(deffunction main-menu ()
   (printout t crlf)
   (printout t "==================================================" crlf)
   (printout t "       MEDICAL LABORATORY TEST ANALYZER" crlf)
   (printout t "==================================================" crlf)
   (printout t "1. Run Example 1: Diabetes case" crlf)
   (printout t "2. Run Example 2: Anemia case" crlf)
   (printout t "3. Run Example 3: Liver disease case" crlf)
   (printout t "4. Run Example 4: Kidney disease case" crlf)
   (printout t "5. Run Example 5: Infection case" crlf)
   (printout t "6. Run Example 6: Thyroid disorder case" crlf)
   (printout t "7. Run Example 7: Lipid profile abnormalities" crlf)
   (printout t "8. Run Example 8: Dehydration case" crlf)
   (printout t "9. Run Example 9: High glucose case" crlf)
   (printout t "10. Run Example 10: Child case" crlf)
   (printout t "11. Create custom test case" crlf)
   (printout t "0. Exit" crlf)
   (printout t crlf)
   (printout t "Enter your choice: ")
   
   (bind ?choice (read))
   
   (if (= ?choice 0) then
      (printout t "Exiting program. Thank you for using the Medical Laboratory Test Analyzer." crlf)
   else (if (= ?choice 11) then
      (custom-test-case)
      (main-menu)
   else (if (and (>= ?choice 1) (<= ?choice 10)) then
      (run-example ?choice)
      (main-menu)
   else
      (printout t "Invalid choice. Please try again." crlf)
      (main-menu)
   )))
)

; Start the system
; To use the system, uncomment the following line:
; (main-menu)

; Alternatively, you can run specific examples directly:
; (run-example 1)  ; Run the diabetes case example
; (custom-test-case)  ; Create your own test case