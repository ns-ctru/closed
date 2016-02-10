shinyUI(
    fluidPage(
        titlePanel("ClosED - Effect of closing Emergency Departments"),
        sidebarPanel(width = 2,
                     h2('Options...'),
                     p('Welcome, you are viewing the results of the ClosED study which seeks to investigate the impact of closing Emergency Departments on local population health.  You can modify which results you can view on each of the tabs using the menus.'),
                     selectInput('hospital',
                                 'Hospital of Interest',
                                 choices = c('Bishop Auckland'  = 'bishop',
                                             'Hartlepool'       = 'hartlepool',
                                             'Hemel Hempstead'  = 'hemel',
                                             'Newark'           = 'newark',
                                             'Rochdale'         = 'rochdale',
                                             'All'              = 'all'),
                                 selected = 'all',
                                 multiple = FALSE),
                     p('Select which condition to display for each Mortality and Emergency Admissions...'),
                     selectInput('mortality',
                                 'Mortality',
                                 choices  = c('Acute Heart Failure'                     = 'ahf',
                                              'Anaphalaxis'                             = 'anaphalaxis',
                                              'Asthma'                                  = 'asthma',
                                              'Cardiac Arrest'                          = 'ca',
                                              'Falls in those < 75'                     = 'falls',
                                              'Fractured Neck of Femur'                 = 'femur',
                                              'Meningitis'                              = 'meningitis',
                                              'Myocradial Infarction'                   = 'mi',
                                              'Non-superficial Head Injuries'           = 'head',
                                              'Pregnancy and Birth Related Conditions'  = 'pregnancy',
                                              'Road Traffic Accidents'                  = 'rta',
                                              'Ruptured Aurtic Aneuyrsm'                = 'aneurysm',
                                              'Self-harm'                               = 'selfharm',
                                              'Septic Shock'                            = 'septicshock',
                                              'Stroke/CVA'                              = 'stroke'),
                                 selected = 'mi',
                                 multiple = FALSE),
                     selectInput('admissions',
                                 'Emergency Admissions',
                                 choices  = c('Acute Mental Crisis'                    = 'mentalcrisis',
                                              'Angina'                                 = 'angina',
                                              'Blocked Urinary Catheter'               = 'urinarycatheter',
                                              'Cellulitis'                             = 'cellulitis',
                                              'Childhood Pyrexial'                     = 'pyrexial',
                                              'Chronic Obstructive Pulmonary Disease'  = 'copd',
                                              'Deep Vein Thrombosis'                   = 'dvt',
                                              'Epileptic Seizure'                      = 'seizure',
                                              'Falls in those > 75'                    = 'falls',
                                              'Hypoglycemia'                           = 'hypoglycemia',
                                              'Non-specific Abdominal Pains'           = 'abdominalpains',
                                              'Non-specific Chest Pains'               = 'chestpaints',
                                              'Superficial Head Injuries'              = 'head',
                                              'Urinary Tract Infections'               = 'uti'),
                                 selected = 'mi',
                                 multiple = FALSE),
                     sliderInput('digits',
                                 'Decimal Places',
                                 min   = 1,
                                 max   = 5,
                                 value = 2)
                     ),
        mainPanel(width = 8,
                  height = 16,
                  column(12,
                         tabsetPanel(tabPanel('Mortality',
                                              tabsetPanel(tabPanel('Mortality Rate'),
                                                          tabPanel('Case Fatality Ratio'))
                                              ),
                                     tabPanel('Emergency Attendance...',
                                              tabsetPanel(tabPanel('Overall'),
                                                          tabPanel('Ambulance and "Other"'),
                                                          tabPanel('Discharges without Treatment'),
                                                          tabPanel('Resulting in Admission'))
                                              ),
                                     tabPanel("Emergency Admissions",
                                              tabsetPanel(tabPanel('Overall'),
                                                          tabPanel('Urgent Conditions'))
                                              ),
                                     tabPanel("Condition Severity",
                                              tabsetPanel(tabPanel('Mean Length of Stay'),
                                                          tabPanel('Proportion Admitted to Critical Care'))
                                              ),
                                     tabPanel("Ambulance Service - Time Time from...",
                                              tabsetPanel(tabPanel('999 Call to Scene'),
                                                          tabPanel('Scene to Hospital'),
                                                          tabPanel('999 Call to Hospital'),
                                                          tabPanel('Arriving at Hospital to "Clear"'))
                                              ),
                                     tabPanel("Summary Statistics",
                                              tabsetPanel(tabPanel('Autocorrelation'),
                                                          tabPanel('Time Series'))
                                              ),
                                     tabPanel("ToDo",
                                              column(6,
                                              fluidRow(p("Everything needs doing, this list isn't exhaustive..."),
                                                       HTML("<ul>
                                                              <li> Data in summary counts by month for each outcome and by each method of allocating to LSOA.
                                                              <li> Write functions for plotting data (unadjusted and adjusted i.e. smoothing for seasonal variation).
                                                              <li> Calculate Auto-correlation.
                                                              <li> Regression objects from Prais-Winsten to be recognised by Stargazer package to ease formatting of results.
                                                            </ul>"))
                                                     )
                                              )
                                     )
                         )
                  )
    )
)
