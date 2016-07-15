shinyUI(
    fluidPage(
        titlePanel("ClosED - Effect of closing Emergency Departments"),
        sidebarPanel(width = 2,
                     h2('Options...'),
                     p('Welcome, you are viewing the results of the ClosED study which seeks to investigate the impact of closing Emergency Departments on local population health.  You can modify which results you can view on each of the tabs using the menus.'),
                     selectInput('hospital',
                                 'Hospital of Interest',
                                 ## ToDo - Make this a list automatically based on data, plus 'All'
                                 ## choices  = c('Bishop Auckland'  = 'bishop',
                                 ##              'Hartlepool'       = 'hartlepool',
                                 ##              'Hemel Hempstead'  = 'hemel',
                                 ##              'Newark'           = 'newark',
                                 ##              'Rochdale'         = 'rochdale',
                                 ##              'All'              = 'all'),
                                 choices  = c('All',
                                              unique(master$group))
                                 selected = 'all',
                                 multiple = FALSE),
                     selectInput('control',
                                 'Control group to compare...',
                                 choices  = c('Single Matched' = 'matched',
                                              'All Unmatched'  = 'all'),
                                 selected = 'matched',
                                 multiple =  FALSE),
                     p('Select which condition to display for Mortality and Emergency Admissions...'),
                     selectInput('mortality',
                                 'Mortality',
                                 ## ToDo - Make this a list automatically based on data, plus 'All'
                                 ## choices  = c('Acute Heart Failure'                     = 'ahf',
                                 ##              'Anaphalaxis'                             = 'anaphalaxis',
                                 ##              'Asthma'                                  = 'asthma',
                                 ##              'Cardiac Arrest'                          = 'ca',
                                 ##              'Falls in those < 75'                     = 'falls',
                                 ##              'Fractured Neck of Femur'                 = 'femur',
                                 ##              'Meningitis'                              = 'meningitis',
                                 ##              'Myocradial Infarction'                   = 'mi',
                                 ##              'Non-superficial Head Injuries'           = 'head',
                                 ##              'Pregnancy and Birth Related Conditions'  = 'pregnancy',
                                 ##              'Road Traffic Accidents'                  = 'rta',
                                 ##              'Ruptured Aurtic Aneuyrsm'                = 'aneurysm',
                                 ##              'Self-harm'                               = 'selfharm',
                                 ##              'Septic Shock'                            = 'septicshock',
                                 ##              'Stroke/CVA'                              = 'stroke'),
                                 choices  = c('All',
                                              unique(filter(master, measure == 'mortality') %>%
                                                     dplyr::select(sub.measure))),
                                 selected = 'mi',
                                 multiple = FALSE),
                     selectInput('admissions',
                                 'Emergency Admissions',
                                 ## ToDo - Make this a list automatically based on data, plus 'All'
                                 ## choices  = c('Acute Mental Crisis'                    = 'mentalcrisis',
                                 ##              'Angina'                                 = 'angina',
                                 ##              'Blocked Urinary Catheter'               = 'urinarycatheter',
                                 ##              'Cellulitis'                             = 'cellulitis',
                                 ##              'Childhood Pyrexial'                     = 'pyrexial',
                                 ##              'Chronic Obstructive Pulmonary Disease'  = 'copd',
                                 ##              'Deep Vein Thrombosis'                   = 'dvt',
                                 ##              'Epileptic Seizure'                      = 'seizure',
                                 ##              'Falls in those > 75'                    = 'falls',
                                 ##              'Hypoglycemia'                           = 'hypoglycemia',
                                 ##              'Non-specific Abdominal Pains'           = 'abdominalpains',
                                 ##              'Non-specific Chest Pains'               = 'chestpaints',
                                 ##              'Superficial Head Injuries'              = 'head',
                                 ##              'Urinary Tract Infections'               = 'uti'),
                                 choices  = c('All',
                                              unique(filter(master, measure == 'mortality') %>%
                                                     dplyr::select(sub.measure))),
                                 selected = 'All',
                                 multiple = FALSE),
                     selectInput('method',
                                 'Choose which Prais-Winsten regression package to use',
                                 choices  = c('panelAR',
                                              'prais'),
                                 selected = 'panelAR',
                                 multiple = FALSE),
                     ## selectInput('mapping',
                     ##             'Mapping',
                     ##             choices  = c('Department of Transport'  = 'dof',
                     ##                          'HES ED Attendance'        = 'hes.ed',
                     ##                          'AS ED Attendance'         = 'as.ed',
                     ##                          'Ambulance Service'        = 'as'),
                     ##             selected = 'dof',
                     ##             multiple = FALSE),
                     checkboxGroupInput('plot.by',
                                        'Plot',
                                        c('Overlay' = 'overlay',
                                          'Facet'   = 'facet')

                     ),
                     sliderInput('digits',
                                 'Decimal Places',
                                 min   = 1,
                                 max   = 5,
                                 value = 2)
                     ),
        mainPanel(width = 8,
                  height = 16,
                  column(12,
                         tabsetPanel(tabPanel('Overview',
                                              tabsetPanel(tabPanel('Background',
                                                                   p('The ClosED study seeks to invsetigate the impact of closing Emergency Departments on local population health by comparing mortality statistics, emergency admissions, mean length of stay and other indicators of relevance to a populations health for catchment areas around five Emergency Departments that closed between 20XX and 20XX.'),
                                                                   h2('Data Sources'),
                                                                   p('Data on hospital admissions, mortality and ambulance conveyance times has been obtained from the following sources'),
                                                                   HTML('<ul>
                                                                           <li> <a href="http://www.hscic.gov.uk/" target="_blank">Health and Social Care Information Commission (HSCIC)</a>
                                                                           <li> <a href="http://www.ons.gov.uk/ons/index.html" target="_blank">Office for National Statistics (ONS)</a>
                                                                           <li> Ambulance Services...
                                                                           <ul>
<li> <a href="http://www.neas.nhs.uk/" target="_blank">North East Ambulance Service NHS Foundation Trust</a>
<li> <a href="http://www.yas.nhs.uk/" target="_blank">Yorkshire Ambulance Service NHS Trust</a>
<li> <a href="http://www.nwas.nhs.uk/" target="_blank">North West Ambulance Service NHS Trust</a>
<li> <a href="http://www.wmas.nhs.uk/Pages/default.aspx" target="_blank">West Midlands Ambulance Service NHS Foundation Trust</a>
<li> <a href="http://www.emas.nhs.uk/Pages/default.aspx" target="_blank">East Midlands Ambulance Service NHS Trust</a>
<li> <a href="http://www.swast.nhs.uk/Pages/default.aspx" target="_blank">South West Ambulance Service NHS Foundation Trust</a>
<li> <a href="http://www.southcentralambulance.nhs.uk/" target="_blank">South Central Ambulance Service NHS Foundation Trust</a>
<li> <a href="http://www.secamb.nhs.uk/" target="_blank">South East Coast Ambulance Service NHS Foundation Trust</a>
<li> <a href="http://www.londonambulance.nhs.uk/" target="_blank">London Ambulance Service NHS Trust</a>
<li> <a href="http://www.eastamb.nhs.uk/" target="_blank">East of England Ambulance Service NHS Trust</a>
                                                                           </ul>
                                                                         </ul>')),
                                                          tabPanel('Data Summary'))
                                              ),
                                     tabPanel('Mortality',
                                              tabsetPanel(tabPanel('Mortality Rate',
                                                                   plotOutput('all'),
                                                                   plotOutput('overlay'),
                                                                   plotOutput('facet')),
                                                          tabPanel('Case Fatality Ratio'))
                                              ),
                                     tabPanel('Attendance...',
                                              tabsetPanel(tabPanel('Overall'),
                                                          tabPanel('Ambulance and "Other"'),
                                                          tabPanel('Discharges without Treatment'),
                                                          tabPanel('Resulting in Admission'))
                                              ),
                                     tabPanel("Admissions",
                                              tabsetPanel(tabPanel('Overall'),
                                                          tabPanel('Urgent Conditions'))
                                              ),
                                     tabPanel("Condition Severity",
                                              tabsetPanel(tabPanel('Mean Length of Stay'),
                                                          tabPanel('Proportion Admitted to Critical Care'))
                                              ),
                                     tabPanel("Ambulance Service",
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
                                              fluidRow(p("Everything needs doing, this list isn't exhaustive..."),
                                                       HTML("<ul>
                                                              <li> Data in summary counts by month for each outcome and by each method of allocating to LSOA.
                                                              <li> Plotting data with smoothing (X-13 with ggseas?).
                                                              <li> Calculate Auto-correlation.
                                                              <li> Regression objects from Prais-Winsten to be recognised by Stargazer package to ease formatting of results (may be easier to use custom).
                                                            </ul>"))
                                              )
                                     )
                         )
                  )
    )
)
