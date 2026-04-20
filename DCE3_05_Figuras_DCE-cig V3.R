################################################################## 
########### Crea figuras para paper DCE Arg, Cl, Col ##########################
################## Agosto 2025 ##############################


rm(list = ls())
# Paquetes utilizados:
library(ggplot2)
library(dplyr)
library(patchwork)
library(gridExtra)
library(stringr)
library(patchwork)


# Definir directorio de trabajo
#setwd("C:/Users/jfino/Dropbox/Tabaco-DCE-2024/Resultados finales/Figuras")
# setwd("D:/Paul.Rodriguez/Dropbox (Personal)/tabaco/Tabaco-DCE-2024/Resultados finales/Figuras")

# Cargamos los datos en data frames: 
# Category
categorias <- c(
  "Disposable ENDS", "Rechargeable ENDS", "Conventional cigarette",
  "HTO: Mildly Harmful", "HTO: Very Harmful", "HTO: Unknown Harm",
  "Self Harm: as harmful as cigarette", "Hide: difficult to hide",
  "Menthol", "Fruit or Candy"
)

# Groups and Age
fumadores <- c("Traditional smokers", "ENDS users-dual", "Non-smokers")

#General
{
  #Create dataset with results obtained from Excel
  df_general <- data.frame(
    Category = rep(categorias, times = 3),
    Group = rep(fumadores, each = 10),
    Population = "General",  
    Mu = c(
      -2.21416018269281, -1.93101175659427, -52.4393678492486, 5.09166421289972, 7.31678277190131, 6.32425861315136, 4.1785042537316, 1.63413152016895, -1.24228262939237, -1.39122572536977,
      -14.3155150198791, -18.5683748225732, -29.9070667538523, 1.91298495040015, 5.40930582135414, 3.69016093664655, 2.387361447559, 0.57937963000969, -1.78174908307434, -1.96287163133353,
      -16.245702547412, -20.7786974352, -15.8937595637786, 8.93686462197647, 18.4721799455626, 10.7283549941983, 8.87130342728393, 2.40664571281262, -1.45225888743049, -1.91337228286339),
    
    SE = c(0.945680716301378, 1.04882496321905, 5.6564635288139, 0.857896587126416, 1.02589209401013, 1.02582404579032, 0.733948699033728, 0.569478145768229, 0.523986787831049, 0.541058231377278,
           1.31142241411304, 1.79314446013229, 2.73255378429375, 0.49019655869079, 0.656845667330514, 0.548444147197967, 0.404892807954591, 0.379089191363016, 0.35725352391341, 0.395960556265605,
           2.34886479445596, 3.19313325398897, 2.9077161757097, 1.51111304210474, 2.87004871286299, 1.73945824947905, 1.44043291769843, 0.56541176016766, 0.498920408851379, 0.553557282883165),
    Sigma = c(
      23.0444271608789, 29.6605998284962, 34.7402689541308, 1.56855190647368, 7.31308653146375, 4.85939786325636, 6.72944929579997, 4.1890801056837, 7.17982072169283, 10.3366397779083,
      12.2341981686045, 18.7312480726558, 11.8187357576732, 2.80796233194299, 7.31878292962496, 4.78004022314571, 4.88011830725787, 4.32444145296608, 4.66808944478714, 5.70632247010692,
      51.1386322613014, 52.9553887419129, 56.8897003842642, 2.19738606910488, 9.03210273669832, 3.39593726701181, 8.79824624295794, 4.84582677421897, 0.500666198372485, 5.32769235123145
 )
  )
}


# Edad
{
  
  edades <- factor(c("Age: 18-25", "Age: 26-64"))
  
  # That block of code creates all possible combinations between categories, groups, and ages, 
  # and then deletes invalid combinations (in this case, a non-smoker being in the 26–64 age group).
  
  
  combinaciones <- expand.grid(
    Category = categorias,
    Group = fumadores,
    Population = edades,
    stringsAsFactors = FALSE
  )
  
  combinaciones <- combinaciones %>%
    filter(!(Group == "Non-smokers" & 
               Population %in% c("Age: 26-64", "Age: 18-25")))
  
  df_Age <- combinaciones %>%
    mutate(
      Mu = c(-2.83631836785569, -4.84623134993358, -53.5809915048513, 8.17973097555775, 5.04077362777475, 7.48317041114218, 6.80270211140984, 3.34633899604276, -4.90297539631969, -6.37739296088657,
             -12.5012557973442, -17.0684498700316, -24.8120638899591, 2.14806155693281, 5.30827850743552, 3.04775612657962, 2.36261747358854, 0.910475766219179, -2.23787958228793, -3.16792859410573,
             0.833080820338568, 1.23045390217445, -60.9899288118643, 5.5552796299941, 8.15498673450817, 5.87247998885165, 4.24616012624708, 0.968410753395007, -0.453206920633832, 1.03381566558055,
             -13.339344799724, -21.4804182544794, -31.6594075324595, 1.31271781335572, 6.46238974004104, 3.60366424486923, 2.58899363251558, -0.00826206448537856, -1.1820179149674, -0.630639849907843
             ), 
      SE = c(2.46502325309157, 3.03221752387526, 16.1240499074398, 3.23104833473865, 2.34647383554611, 2.81137144681957, 2.42840561751058, 1.79123443354065, 2.04090685052517, 2.47392670784281,
             1.76311738491071, 2.4624593375746, 3.49373991302488, 0.737039557336983, 0.91942709399126, 0.736715896011215, 0.607426296249808, 0.52292019694157, 0.581403919153044, 0.704761529537073,
             1.61292441100919, 1.51667299796955, 7.96578551213706, 1.1230878736886, 1.29165677881659, 1.14225576806906, 0.847802069910663, 0.660398273042998, 0.628903548742395, 0.64966097297604,
             1.45350265178457, 2.37606140976942, 3.45354172645363, 0.506709090832778, 0.860227776475932, 0.576176932848895, 0.477127887665991, 0.450099777991671, 0.416588092660361, 0.398598769198587),
      
      Sigma = c(31.9494526044633, 39.1395492482246, 6.72807888775618, 8.24847173073698, 20.6206631404919, 1.24749989701914, 5.29994187618539, 8.28212716958583, 8.31502350750528, 14.1752475423254,
                10.9906201814394, 15.0649583161934, 17.2070888346077, 2.31804946849664, 5.25915214119849, 5.07251592467723, 4.37340419891204, 1.92468420858895, 3.90357666518307, 5.90964657145391,
                24.8673985489675, 33.8981947342006, 43.2035124251572, 1.87852532636001, 4.64606177780999, 1.59123462293247, 2.93779927837355, 1.27741587475768, 6.94122497987244, 9.9506296464923,
                12.7358943843933, 14.1760381822975, 17.482251810278, 2.1604820758066, 2.92564171466833, 1.91591432121154, 2.82986184321243, 4.67919293174091, 3.63836743861924, 3.6164821387741) 
    )
}
# Country
{
  paises <- c("Argentina", "Chile", "Colombia")
  
  paises_rep <- rep(rep(paises, each = length(categorias)), times = length(fumadores))
  grupos_rep <- rep(fumadores, each = length(categorias) * length(paises))
  
  
  # Create all the correct combinations of Country, Group, and Category.
  combinaciones <- expand.grid(
    Category = categorias,
    Group = fumadores,
    Population = paises
  )
  
  # Reorder so that it is first by Country, then Group, then Category
  combinaciones <- combinaciones %>% 
    arrange(Population, Group, Category)
  
  # Create dataframe
  df_Country <- combinaciones %>%
    mutate(
      Mu = c(-1.30507349068794, 1.15222012265485, -60.770268680429, 6.02096269581683, 10.0033085387899, 5.27296732500865, 3.86959715744321, -2.09765899368286, 1.31040454672406, 1.38662038020367,
             -11.54754424, -15.9220313, -27.58011137, 2.496966774, 4.844068366, 3.44281957, 1.665807164, 0.655185912, -1.439652773, -1.483074147,
             -25.5195012826271, -24.3583458154396, -27.145653934857, 8.86326835723848, 18.54424163944, 9.56958466465121, 8.90880992781598, 3.35250775365601, -3.03644128648984, -4.56158512355649,
             -6.20765364857449, 0.567214597784417, -73.4322546281721, 7.63761814859436, 10.8631743917415, 9.03413036787825, 5.16740772771099, 3.20778867584877, -4.0783384657676, -6.37839265466004,
             -18.4998648784236, -18.5802998186713, -18.6166834119474, 10.4085594767467, 21.4269425865648, 14.204050977926, 10.6433167437223, 1.91205641399728, -2.12373014493017, -7.31403882184822,
             -8.7411389478534, -18.5802998186713, -18.6166834119474, 10.4085594767467, 21.4269425865648, 14.204050977926, 10.6433167437223, 1.91205641399728, -2.12373014493017, -7.31403882184822,
             -3.617225193, -0.809490124, -33.786318, 4.391613358, 3.477211135, 2.447795254, 3.291470927, 1.328785613, -0.369629456, -0.306231903,
             -13.28371051, -18.97564288, -25.52885147, 0.884833073, 5.184772185, 3.537069363, 3.305356623, 0.469431705, -0.976071162, -1.194371593,
             0, -11.5663224179831, 19.1399940224049, 15.7598914585253, 30.8303549890679, 18.0563101279001, 11.3101595389471, 2.17066122770207, -0.702372809391305, -0.026723911839145), 
      SE = c(1.84380586115076, 2.24121285991284, 12.0657333218759, 1.81659150058783, 2.24497193944836, 1.77637077741722, 1.433354939246, 1.30382372124325, 1.19819843110629, 1.10252189295479,
             1.96603706, 2.7067904, 4.35574264, 0.94383006, 1.1320615, 0.97800942, 0.72946702, 0.75231458, 0.71567642, 0.66195454,
             8.45713493259672, 8.45613761566949, 9.01884569579473, 3.3052407299601, 6.23579663880258, 3.51016771219669, 3.22501114787454, 1.52711437333322, 1.57317753137132, 1.91649539281386,
             1.99789003349162, 2.39888820049393, 17.3453442265247, 2.26363264570117, 2.92641514476127, 2.70398454715527, 1.41656825907124, 1.11740450667492, 1.37973730181284, 1.84265890470183,
             4.20393293005887, 6.0318339444231, 10.5678657353898, 1.31591580465827, 2.47524286401339, 1.50400645431824, 1.29013698786915, 1.00445261161957, 1.06220075547698, 1.36346412778449, 
             2.85749299782893, 5.90926641945614, 6.56408408294261, 3.67607639307526, 6.8814568701088, 4.60014691950737, 3.45764276172576, 1.13035082957174, 1.23606189307253, 2.53623965148625,
             1.39523276, 1.18076934, 4.34678924, 0.95742858, 0.9163409, 0.8309766, 0.81830724, 0.64898238, 0.57729702, 0.58842808,
             1.87101226, 2.66789204, 3.62120346, 0.60295882, 0.95386916, 0.74073612, 0.667599, 0.51708284, 0.46963608, 0.4977738,
             7.25845118104039, 4.78488049933669, 11.2869175024665, 6.87477719310005, 12.9494656552325, 7.6908565402424, 4.64731600003973, 1.26095570494626, 1.35109314127177, 1.24217686479232),
      
      Sigma = c(32.5454554451595, 38.4792309889783, 41.5881257319651, 12.1986751943424, 0.83918031166136, 0.326878483148695, 12.8700023244148, 9.91361526772021, 10.8701370224078, 16.040761560976,
                16.0166552, 16.48122946, 9.991579787, 4.801262791, 6.628886867, 3.758090238, 4.045444701, 6.010259382, 6.171127569, 7.083986863,
                56.7740380276955, 73.1135463544013, 68.2808387849418, 0.984595209563811, 10.2418308488219, 4.11470439010417, 8.18068935839198, 4.98798287481051, 3.31878968714572, 8.54679735764203,
                34.6290891933222, 41.1099345831828, 50.3480978908661, 4.45363998688282, 11.3151734243233, 2.62426418997665, 3.60283154731751, 1.13537469661696, 5.86527159688986, 12.1608551766672,
                17.8856051196288, 45.5153058546916, 1.50479496107634, 4.94333559501957, 11.7873282803851, 8.4842132365538, 9.6220783227727, 0.0931603703862613, 4.58777641112053, 7.64231658896217,
                55.0556667156125, 18.6166834119474, 39.5118285175578, 3.04601715799283, 22.9616934923045, 10.9328454704717, 16.1517897234066, 5.1605320364554, 4.50279741898756, 11.8557811501254,
                16.11840768, 20.76691521, 26.11550101, 3.324615456, 4.831549499, 0.436654755, 3.959315776, 4.099980556, 0.678444789, 3.201015869,
                12.63742724, 13.49057024, 12.12077718, 3.066726265, 5.896855427, 3.389267433, 2.778244129, 3.391751286, 1.836565877, 2.848869034,
                61.5930018627564, 75.4391640981209, 75.3402405015993, 2.16522502666567, 25.0992616462102, 7.82694590278409, 18.761701759568, 12.2964791227828, 2.12166827802525, 3.51280786461976
                ))
}



# Gender
{
  sexo <- c("Woman", "Man")
  combinaciones <- expand.grid(
    Category = categorias,
    Group = fumadores,
    Population = sexo,
    stringsAsFactors = FALSE
  )
  
  
  df_gender <- combinaciones %>%
    mutate(
      Mu = c(-1.63621933881482, 0.201112794160145, -51.2354557072981, 5.10195954226569, 5.04793872369401, 8.11211392822827, 5.3111631621081, 1.62004867682029, -2.22699718667793, -3.72490424503987,
             -15.569727286472, -19.8193423535096, -29.1563299809484, 1.86597726592342, 6.75670115835047, 4.18032024499957, 2.76960628915135, 0.280535736892019, -2.75665100862314, -3.60259676531122,
             -23.6827625092707, -13.036620940828, -20.1482425204886, 11.439029418883, 22.752698472719, 13.4297124304578, 12.1512014309941, 3.52588942363419, -0.918401408051904, -2.7674651979327,
             0.817701293432273, 3.48743780172377, -40.0771913007657, 3.9015872782579, 3.04804504552067, 2.68745759482563, 3.43796788129112, 0.185850077824601, 0.506609880001672, 1.68421733774178,
             -15.5978523157066, -22.2559680307466, -31.2181844944673, 2.01318276578361, 6.15595184052617, 3.44073776384959, 1.83159574396495, 0.471365953902738, 0.23228395685873, -0.15259564601443,
             -6.04289287989278, -11.9208033032332, -9.9401579662895, 6.3081832241518, 11.9235598821405, 7.22861281106792, 5.34912792732776, 0.21143072069369, -0.588151665586225, -1.72417635119485
      ), 
      SE = c(1.46172799585231, 1.67189327192605, 8.35443564518571, 1.22942983017003, 1.36182377573755, 1.75123393153641, 1.26612794358377, 0.848389404359911, 0.861321763329433, 1.03841512445317,
             4.67809039407452, 2.93735612737024, 5.05947325585399, 2.55579976906906, 4.91741558177562, 2.86833166262292, 2.60652260323795, 1.0218557508076, 0.701592260242127, 0.918808898363428,
             4.67809039407452, 2.93735612737024, 5.05947325585399, 2.55579976906906, 4.91741558177562, 2.86833166262292, 2.60652260323795, 1.0218557508076, 0.701592260242127, 0.918808898363428,
             1.54226375138049, 2.12008459269544, 6.02307505460644, 1.30755289889703, 1.27678221171989, 0.997168462665802, 1.00215706759528, 0.994981253007309, 0.648235584359715, 0.678137021469096,
             2.24955473016609, 3.19210695254647, 4.42138972803285, 0.720315475167265, 1.00717240979833, 0.770176295744426, 0.554273403905518, 0.598328682391493, 0.557770844621621, 0.491730986259459,
             1.07181559341733, 1.72112513928031, 2.63732583253017, 1.0242483773178, 1.81611385293178, 0.887929188599839, 0.942977320295842, 0.425375869579526, 0.474135814642633, 0.587198538607292),
      
      Sigma = c(25.6662900955718, 32.1114247803222, 29.5657708752447, 0.439538148585911, 15.0472230531794, 7.26328800965141, 8.2883704384404, 4.35121641563995, 7.89437181278707, 13.9227450175114,
                13.9160919336623, 17.4753691086243, 17.9489378869081, 3.27160278549954, 6.63755413369829, 4.12500655821291, 3.91174295278879, 4.16268827717671, 5.12276596718106, 6.20663020628406,
                61.0864763246526, 61.4176105563889, 58.7843138681327, 3.95539313773596, 20.1813949021277, 4.31862345513638, 9.24203795005922, 9.88941581300941, 0.169363105147247, 4.73604596691766,
                19.8804485414873, 0.273229874150826, 0.177537767865269, 0.0781261789508332, 0.0860841783562123, 0.0720729433800067, 0.0526375277716161, 0.0672442877556708, 0.0440021515260331, 0.0560472614421842,
                17.9489378869081, 1.86597726592342, 3.27160278549954, 4.12500655821291, 3.91174295278879, 4.16268827717671, 5.12276596718106, 6.20663020628406, 13.0257673182346, 9.3369876274388,
                58.7843138681327, 11.439029418883, 3.95539313773596, 4.31862345513638, 9.24203795005922, 9.88941581300941, 0.169363105147247, 4.73604596691766, 16.3842630494361, 7.11162157966053) 
    )
}


# Unimos todos en un solo data frame
df_all <- bind_rows(df_general, df_Age, df_Country, df_gender)
df_all$Mu= - df_all$Mu  # Para que sea más fácil de interpretar

# Estas categoría la dejamos en negativo para poder expresar más fácil su significado
df_all$Mu[df_all$Category=="Hide: difficult to hide"]           =-df_all$Mu[df_all$Category=="Hide: difficult to hide"]

df_all$Category[df_all$Category=="Hide: difficult to hide"]="Easy to hide"


#Comprobamso que sean únicos 
print(any(duplicated(df_all %>% select(Category, Group, Population)))) #No hay duplicados

# Creamos IC al 95% 

df_all <- df_all %>%
  mutate(
    lower = Mu - 1.96 * SE,
    upper = Mu + 1.96 * SE
  )

# Graficamos todo: 

colores_grupos <- c(
  "Traditional smokers"= "#c77cff",
  "ENDS users-dual"= "#f8766d", 
  "Non-smokers"= "#00bfc4"
)

rm(df_general, df_Age, df_Country, df_gender)
###########################  Graficos  ####################################### 

#CAT: 
#  "Disposable ENDS", "Rechargeable ENDS", "Conventional cigarette",
#  "HTO: Mildly Harmful", "HTO: Very Harmful", "HTO: Unknown Harm",
#  "Self Harm: as harmful as cigarette", "Hide: difficult to hide",
#  "Menthol", "Fruit or Candy"

plot_dot_error <- function(df, grupo = "Traditional smokers", categorias_seleccion = c("Disposable ENDS", "Rechargeable ENDS", "Conventional cigarette"), mostrar_y = 1, limites_x = c(20, 300)) {
  
  # Filtramos según grupo y categorías
  df_plot <- df %>%
    filter(
      Group == grupo,
      Category %in% categorias_seleccion
    ) %>%
    mutate(
      # Convertimos Population en factor y controlamos el orden
      Population = factor(Population, levels = c("Colombia", "Chile", "Argentina", "Man", "Woman", "Age: 26-64", "Age: 18-25", "General"))
    )
  
  # Crear colores dinámicamente según la cantidad de categorías seleccionadas
  colores <- scales::hue_pal()(length(categorias_seleccion))
  names(colores) <- categorias_seleccion
  
  # Crear gráfico
  ggplot(df_plot, aes(x = Mu, y = Population, color = Category)) +
    geom_errorbarh(aes(xmin = lower, xmax = upper),
                   width = 0.4, linewidth = 1.1,  
                   position = position_dodge(width = 0.9)) +
    geom_point(size = 3.5, stroke = 1, shape = 16,
               position = position_dodge(width = 0.9)) +
    geom_text(
      aes(label = round(Mu, 1), x = upper),  # coloca el texto al final del CI
      hjust = -0.2,                          # un poco a la derecha del extremo
      vjust = 0.5,
      size = 5,                              # tamaño visible
      show.legend = FALSE,
      position = position_dodge(width = 0.9)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
    scale_y_discrete(expand = expansion(mult = c(0.15, 0.1))) +
    scale_x_continuous(limits = limites_x) + 
    scale_color_manual(values = colores) +
    labs(title =  grupo,
         x = "Willingness to Pay",
         y =  NULL,
         color = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = if (mostrar_y == 3) "bottom" else "none",
      legend.justification = if (mostrar_y == 3) "left" else "none" ,
      legend.title = element_text(size = 11) , 
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(face = "bold", size = 11, hjust = 0),
      axis.text.y = if (mostrar_y == 1 | mostrar_y == 3) element_text(face = "bold", size = 11, hjust = 1) else element_blank()
      #axis.text.y = element_text(face = "bold", size = 11, hjust = 1)
    )
}


###########################  Graficos: Alternativas  ####################################### 
plot_dot_error(df_all, grupo = "Traditional smokers", categorias_seleccion = c("Disposable ENDS", "Rechargeable ENDS", "Conventional cigarette"), mostrar_y = 1, limites_x = c(-50, 100) )

plot_alt_TS <- plot_dot_error(df_all, grupo = "Traditional smokers", categorias_seleccion = c("Disposable ENDS", "Rechargeable ENDS", "Conventional cigarette"), mostrar_y = 1, limites_x = c(-50, 150) )
plot_alt_dual <- plot_dot_error(df_all, grupo = "ENDS users-dual", categorias_seleccion = c("Disposable ENDS", "Rechargeable ENDS", "Conventional cigarette"), mostrar_y = 2,  limites_x = c(-50, 100))
plot_alt_NS <- plot_dot_error(df_all, grupo = "Non-smokers", categorias_seleccion = c("Disposable ENDS", "Rechargeable ENDS", "Conventional cigarette"), mostrar_y = 3, limites_x = c(-50, 100) )

(plot_alt_TS | plot_alt_dual | plot_alt_NS) + plot_layout(ncol = 2, nrow = 2)
plot_alternatives <- (plot_alt_TS | plot_alt_dual | plot_alt_NS) + plot_layout(ncol = 2, nrow = 2)


ggsave("Alternatives.png", plot_alternatives, width = 10, height = 14, dpi = 450)
ggsave("Alternatives.svg", plot_alternatives, width = 10, height = 14, dpi = 450,device = "svg")

###########################  Graficos: Atributos  ####################################### 

##### Harm 
plot_harm_TS <- plot_dot_error(df_all, grupo = "Traditional smokers", categorias_seleccion = c("HTO: Mildly Harmful", "HTO: Very Harmful", "HTO: Unknown Harm", "Self Harm: as harmful as cigarette"), mostrar_y = 1, limites_x = c(-60, 20))
plot_harm_dual <- plot_dot_error(df_all, grupo = "ENDS users-dual", categorias_seleccion = c("HTO: Mildly Harmful", "HTO: Very Harmful", "HTO: Unknown Harm", "Self Harm: as harmful as cigarette"), mostrar_y = 2, limites_x = c(-60, 20))
plot_harm_NS <- plot_dot_error(df_all, grupo = "Non-smokers", categorias_seleccion = c("HTO: Mildly Harmful", "HTO: Very Harmful", "HTO: Unknown Harm", "Self Harm: as harmful as cigarette"), mostrar_y = 3, limites_x = c(-60, 20))



(plot_harm_TS | plot_harm_dual | plot_harm_NS) + plot_layout(ncol = 2, nrow = 2)
plot_harm <- (plot_harm_TS | plot_harm_dual | plot_harm_NS) + plot_layout(ncol = 2, nrow = 2)
ggsave("Harm.png", plot_harm, width = 10, height = 14, dpi = 300)
ggsave("Harm.svg", plot_harm, width = 10, height = 14, dpi = 300,device = "svg")

##### Hide
plot_hide_TS <- plot_dot_error(df_all, grupo = "Traditional smokers", categorias_seleccion = c( "Easy to hide" ), mostrar_y = 1, limites_x = c(-10, 10))
plot_hide_dual <- plot_dot_error(df_all, grupo = "ENDS users-dual", categorias_seleccion = c( "Easy to hide" ), mostrar_y = 2, limites_x = c(-10, 10))
plot_hide_NS <- plot_dot_error(df_all, grupo = "Non-smokers", categorias_seleccion = c( "Easy to hide" ), mostrar_y = 3, limites_x = c(-10, 10))


(plot_hide_TS | plot_hide_dual | plot_hide_NS) + plot_layout(ncol = 2, nrow = 2)
plot_hide <- (plot_hide_TS | plot_hide_dual | plot_hide_NS) + plot_layout(ncol = 2, nrow = 2)
ggsave("Hide.png", plot_hide, width = 10, height = 14, dpi = 300)
ggsave("Hide.svg", plot_hide, width = 10, height = 14, dpi = 300,device = "svg")



##### Flavours
plot_flavours_TS <- plot_dot_error(df_all, grupo = "Traditional smokers", categorias_seleccion = c("Menthol", "Fruit or Candy"), mostrar_y = 1, limites_x = c(-5, 15))
plot_flavours_dual <- plot_dot_error(df_all, grupo = "ENDS users-dual", categorias_seleccion = c( "Menthol", "Fruit or Candy"), mostrar_y = 2, limites_x = c(-5, 15))
plot_flavours_NS <- plot_dot_error(df_all, grupo = "Non-smokers", categorias_seleccion = c("Menthol", "Fruit or Candy"), mostrar_y = 3, limites_x = c(-5, 15))


(plot_flavours_TS | plot_flavours_dual | plot_flavours_NS) + plot_layout(ncol = 2, nrow = 2)
plot_flavours <- (plot_flavours_TS | plot_flavours_dual | plot_flavours_NS) + plot_layout(ncol = 2, nrow = 2)
ggsave("Flavours.png", plot_flavours, width = 10, height = 14, dpi = 300)
ggsave("Flavours.svg", plot_flavours, width = 10, height = 14, dpi = 300,device = "svg")



# Figure 1: attribute importance, for all smokers =========================
# Importante: no se incluyen los precios porque los coeficientes asociados
# son los parametros dentro de la distribución -LogNormal que siguen
# En consecuencia, no son fácilmente comparables con el resto de coeficientes
# Paquetes
library(tidyverse)
library(patchwork)


# Input data

df1 <- tribble(
  ~Parameter, ~Coefficient, ~Se, ~Mu,
  
  "ASC: disponsable ENDS",              1.487666, 0.08755, 1,
  "ASC: disponsable ENDS",              2.034805, 0.098214, 0,
  
  "ASC: rechargeable ENDS",             2.093872, 0.099414, 1,
  "ASC: rechargeable ENDS",            -2.72899,  0.11386,  0,
  
  "ASC: conventional cigarette",        5.274346, 0.199197, 1,
  "ASC: conventional cigarette",       -4.03866,  0.158182, 0,
  
  "Price in cents USD per unit (1 cig / 10 puff)[NEG]",                             -6.09538,  0.738628, 1,
  "Price in cents USD per unit (1 cig / 10 puff)[NEG]",                              7.068809, 0.505073, 0,
  
  "Mildly harmful [NEG]",              -0.36197,  0.053484, 1,
  "Mildly harmful [NEG]",              -0.57473,  0.092353, 0,
  
  "Very harmful [NEG]",                -0.80007,  0.057567, 1,
  "Very harmful [NEG]",                -0.82041,  0.05795,  0,
  
  "Unknown harmful [NEG]",             -0.54187,  0.051876, 1,
  "Unknown harmful [NEG]",              0.363655, 0.126421, 0,
  
  "As harmful as cigarette [NEG]",     -0.45437,  0.042208, 1,
  "As harmful as cigarette [NEG]",      0.63573,  0.064287, 0,
  
  "Difficult to hide [NEG]",           -0.12735,  0.039851, 1,
  "Difficult to hide [NEG]",           -0.51565,  0.067908, 0,
  
  "Flavor: menthol",                    0.180221, 0.040538, 1,
  "Flavor: menthol",                    0.746132, 0.057635, 0,
  
  "Flavor: fruit or candy",             0.234854, 0.040324, 1,
  "Flavor: fruit or candy",            -0.97034,  0.049682, 0
)



# Si quisiéramos poner el precio, deberíamos obtener los momentos
# de la distribución... no me lle´go a funcionar bien. Dejo
# aca lo último
# 1) Extrae momentos para precio (mu_p, sd_p) y recupera (m,s)
#row_mu_p <- df1 %>% filter(Mu==1, str_detect(Parameter, "^Price")) %>% slice(1)
#row_sd_p <- df1 %>% filter(Mu==0, str_detect(Parameter, "^Price")) %>% slice(1)
#mu_p <- as.numeric(row_mu_p$Coefficient)        # ~ -13.358...
#sd_p <- as.numeric(row_sd_p$Coefficient)        # ~ 8.702

#s2  <- log(1 + (sd_p/abs(mu_p))^2)
#s   <- sqrt(s2)
#m   <- log(abs(mu_p)) - 0.5*s2   # ~2.415


# df1: Parameter, Coefficient, Se, Mu (1 = mu, 0 = sigma)
stopifnot(all(c("Parameter","Coefficient","Se","Mu") %in% names(df1)))

df_plot <- df1 %>%
  mutate(
    kind = if_else(Mu == 1, "Coefficients (μ)", "Variability (σ)"),
    group = case_when(
      str_detect(Parameter, "^ASC")                         ~ "Alternatives (ASC)",
      str_detect(Parameter, "(?i)Price")                    ~ "Price",
      str_detect(Parameter, "(?i)^Difficult")                    ~ "Concealability",
      str_detect(Parameter, "(?i)^Flavor")                  ~ "Flavour",
      TRUE ~ "Harm"
    ),
    ci_l = Coefficient - 1.96*Se,
    ci_u = Coefficient + 1.96*Se,
    neg_flag = str_detect(Parameter, fixed("[NEG]"))
  ) %>%
  # 1) Flip para μ con [NEG]
  mutate(
    Coefficient = if_else(kind == "Coefficients (μ)" & neg_flag, -Coefficient, Coefficient),
    ci_l        = if_else(kind == "Coefficients (μ)" & neg_flag, -ci_l,        ci_l),
    ci_u        = if_else(kind == "Coefficients (μ)" & neg_flag, -ci_u,        ci_u)
  ) %>%
  # Reordenar extremos si quedaron invertidos
  mutate(
    tmp_l = pmin(ci_l, ci_u),
    tmp_u = pmax(ci_l, ci_u),
    ci_l  = tmp_l, ci_u = tmp_u
  ) %>%
  select(-tmp_l, -tmp_u) %>%
  # 2) σ en magnitud positiva
  mutate(
    Coefficient = if_else(kind == "Variability (σ)", abs(Coefficient), Coefficient),
    ci_l = if_else(kind == "Variability (σ)", pmax(0, abs(ci_l)), ci_l),
    ci_u = if_else(kind == "Variability (σ)", abs(ci_u), ci_u)
  ) %>%
  # Ordenar dentro de cada facet después de transformar
  group_by(kind) %>%
  mutate(Parameter_f = forcats::fct_reorder(Parameter, Coefficient)) %>%
  ungroup()


# Gráfico único con facets (evita duplicar leyenda y problemas de recorte)
p <- ggplot(df_plot, aes(x = Parameter_f, y = Coefficient, fill = group)) +
  geom_col(width = 0.75) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.15) +
  coord_flip(clip = "off") +
  facet_wrap(~ kind, ncol = 2, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL, y = "Estimate", fill = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )

p

# Exporta
ggsave("Figure1_bars.svg", p, width = 14, height = 10, device = "svg")
ggsave("Figure1_bars.png", p, width = 14, height = 10, dpi = 300)

