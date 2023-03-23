# Box imports -----------------------------------------------------------------

box::use(
  shiny[tagList, tags, br, hr],
  checkmate[assert_string, assert_subset],
  shiny.blueprint[H4, Text, UL, Callout, H6, Code],
)

# -----------------------------------------------------------------------------

#' @export
#' @examples
#' prep_vac_txt("BCG")
prep_vac_txt <- function(vac, ls_vac_text = drawer_txt) {
  assert_string(vac)
  assert_subset(vac, choices = names(ls_vac_text))

  ls_vac_text[[vac]]
}

#' @export
drawer_txt <- list(
  "MCV1" = tagList(
    H4("Measles vaccine"),
    Text("Measles is a highly contagious virus-caused disease that, despite a safe and effective
       vaccine being available, infects thousands of people globally. Around 90,000 people
       die because of measles every year, the huge majority of whom (87%) are children younger
       than 5. Measles vaccination has resulted in an 84% drop in measles deaths between 2000-2016."),
    Text("See also: 'http://www.who.int/news-room/fact-sheets/detail/measles'")
  ),
  "BCG" = tagList(
    H4("Tuberculosis vaccine"),
    Text("Developed more than a century ago, bacille Calmette–Guérin vaccine (BCG) is currently
          the only vaccine available against tuberculosis (TB). Close to 4 billion people have been
          vaccinated with BCG – this makes it the most widely used vaccine in the world."),
    br(),
    Text("BCG vaccine is based on a reduced-virulence strain of Mycobacterium bovis,
         a bacterium that is closely related to Mycobacterium tuberculosis – the pathogen that
         causes TB. BCG vaccine was already used in the 1920s in Belgium and France in small
         trials and after the Second World War its use was expanded to vaccinate children
         across Europe. In 1950s the WHO started recommending the use of BCG globally."),
    hr(),
    H4("The most deadly VPDs (The vaccine-preventable diseases)"),
    Text("Determining which are the most deadly VPDs requires acknowledging that some vaccinations
         are more effective than others. While most vaccines included in national routine schedules
         are highly effective, a better vaccine for tuberculosis (TB) is desperately needed.
         It is Sub-Saharan Africa and parts of Asia in particular that are affected by TB."),
    br(),
    Text("The vaccine for TB – Bacillus Calmette–Guérin (BCG) – has been in use for nearly 100 years.
        It protects against severe forms of TB but is not effective against pulmonary TB (in the lungs)
        (see: 'https://wellcome.org/news/world-immunisation-week-seven-vaccine-challenges')
         and has variable effectiveness against TB in adults."),
    Text("Furthermore, resistance of antibiotics used to treat TB is increasing, meaning some people
         can no longer be cured by drugs. The estimated number of people in 2016 with multi-drug resistant TB was 490,000.
         (see: 'https://www.who.int/news-room/questions-and-answers/item/tuberculosis-multidrug-resistant-tuberculosis-(mdr-tb)')"),
    Text("The fact that there is not a fully effective vaccine and that antibiotic treatment
         is facing serious difficulties makes TB the most deadly VPD.")
  ),
  "Pol3" = tagList(
    H4("Polio vaccine"),
    Text("Poliomyelitis (OPV, IPV) is a highly infectious viral disease. Once the poliovirus invades
         the nervous system it can cause irreversible paralysis in a matter of hours. No cure exists
         for polio, only treatment to alleviate symptoms. The world is on its way to eradicate
         the disease thanks to the vaccine against the virus: While in the 1980s there were 350,000
         paralytic cases of polio every year, the world saw only 42 cases in 2016."),
    br(),
    Text("More about polio eradication 'https://ourworldindata.org/eradication-of-diseases'")
  ),
  "HepB3" = tagList(
    H4("Hepatitis B vaccine"),
    Text("Hepatitis B (HepB) is a highly contagious viral infection that attacks the liver and
         is transmitted through contact with the blood or other body fluids of an infected person.
         It is estimated that about 100,000 people die each year of chronic liver disease due to hepatitis B.
         The WHO recommends that all infants should receive their first dose of vaccine
         as soon as possible after birth, preferably within 24 hours."),
    hr(),
    H4("The most deadly VPDs (The vaccine-preventable diseases)"),
    Text("Determining which are the most deadly VPDs requires acknowledging that some vaccinations are more effective than others.
         The vaccine against hepatitis B is also very effective (an estimated 95% effectiveness
         in preventing disease), meaning increased vaccination could lead to a significant
         reduction in deaths. Still, global coverage of the hepatitis B vaccine is lagging.
         In 2015, while 84% coverage was reached for the third dose of the vaccine,
         the coverage for the birth dose was only 39%. Estimates from mathematical models
         have shown that if infant coverage would reach 90%, and the first dose administered
         at birth, 84% of global hepatitis B-related deaths could be prevented.20")
  ),
  "DTP3" = tagList(
    H4("Diphtheria, Tetanus, and Pertussis vaccine"),
    Text("Diphtheria, tetanus, and pertussis are all bacterial diseases and a combination
       vaccine against all three diseases is commonly used."),
    UL(tags$li("Diphtheria primarily infects the throat and upper airways and is fatal in 5 – 10% of cases.")),
    UL(tags$li("Tetanus is not passed person-to-person but through spores of a bacteria living
               in soil and animal intestinal tracts. These bacteria enter the body through
               wounds and release a toxin that affects the nerves, which causes muscle stiffness and spasms.")),
    UL(tags$li("Pertussis is a highly contagious disease of the respiratory tract,
               commonly known as whooping cough. Children who contract pertussis tend to
               have coughing spells that last four to eight weeks, but the highest fatality is
               in young infants. Vaccinating health workers and pregnant women is the most effective
               strategy for preventing disease in infants too young to be vaccinated.")),
    br(),
    br(),
    Callout(
      title = "Red Alert",
      intent = "danger",
      "The percentage of children who received three doses of the vaccine against diphtheria,
      tetanus and pertussis (DTP3) – a marker for immunization coverage within and across countries
      – fell 5 percentage points between 2019 and 2021 to 81 per cent.",
      hr(),
      "More: 'https://www.who.int/news/item/15-07-2022-covid-19-pandemic-fuels-largest-continued-backslide-in-vaccinations-in-three-decades'"
    )
  ),
  "RotaC" = tagList(
    H4("Rotavirus vaccines"),
    Text("The first widely-used rotavirus vaccine was approved in the United States in 2006.
       Today, there are four oral rotavirus vaccines recommended for use by the World
       Health Organisation (WHO): Rotarix, RotaTeq, RotaSiil, and Rotavac."),
    Text("See also: 'https://www.who.int/immunization/diseases/rotavirus/en/'"),
    br(),
    Text("Since the use of rotavirus vaccines have been approved, they have had
       a notable impact on the reduction of rotavirus-related deaths. According to a study
       published in 2018, the use of rotavirus vaccines prevented approximately 28,900 child deaths globally in 2016. ")
  ),
  "PCV3" = tagList(
    H4("Pneumococcal vaccines"),
    Text("There are a number of ways we could reduce the number of children dying from pneumonia,
         including eliminating the major risk factors such as undernutrition and air pollution,
         and providing better access to treatment."),
    Text("But we have another highly effective intervention: a vaccine against the major
         pathogen responsible for pneumonia in children."),
    br(),
    Text("Streptococcus pneumoniae is the leading cause of pneumonia in children under 5 —
         it was responsible for 52% of all fatal pneumonia cases in children in 2016.
         Pneumococcal vaccines are vaccines that target S. pneumoniae bacteria.")
  )
)

#' @export
drawer_questions <- list(
  H6("Q20 Which of the following people do you trust for medical/health advice?"),
  Text("Family/friends | Religious leader | Doctor/nurse | Famous person | Traditional healer | Don't know/refused | N/A"),
  br(),
  H6("Q21 In general, how much do you trust medical and health advice from the government of this country?"),
  Text("A lot | Some | Not much | Not at all | Don't know | Refused"),
  br(),
  H6("Q22 In general, how much do you trust medical and health advice from medical workers,
     such as doctors and nurses, in this country?"),
  Text("A lot | Some | Not much | Not at all | Don't know | Refused"),
  br(),
  H6("Q23 A vaccine is given to people to strengthen their body's ability to fight certain diseases.
     Sometimes people are given a vaccine as [insert country equivalent for shot or an injection],
     but vaccines can also be given by mouth or some other way.
     Before today, had you ever heard of a vaccine?"),
  Text("Yes | No | Don't know | Refused"),
  br(),
  Callout(
    intent = "primary",
    icon = "warning-sign",
    "The following Vaccine questions were only asked to respondents who answered ",
    Code("Yes"), " to the question above."
  ),
  br(),
  H6("Q24 Do you strongly or somewhat agree, strongly or somewhat disagree or neither
     agree nor disagree with the following statements?'Vaccines are important for children to have'."),
  Text("Strongly agree | Somewhat agree | Neither agree nor disagree | Somewhat disagree | Strongly disagree | Don't know/refuse | squere"),
  br(),
  H6("Q25 Do you strongly or somewhat agree, strongly or somewhat disagree
     or neither agree nor disagree with the following statement? 'Vaccines are safe.'"),
  Text("Strongly agree | Somewhat agree | Neither agree nor disagree | Somewhat disagree | Strongly disagree | Don't know/refuse | squere"),
  br(),
  H6("Q26 Do you strongly or somewhat agree, strongly or somewhat disagree or
     neither agree nor disagree with the following statement? 'Vaccines are effective'"),
  Text("Strongly agree | Somewhat agree | Neither agree nor disagree | Somewhat disagree | Strongly disagree | Don't know/refuse | squere")
)
