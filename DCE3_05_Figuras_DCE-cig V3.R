################################################################## 
########### Crea figuras para paper DCE Arg, Cl, Col ##########################
################## Agosto 2025 ##############################


rm(list = ls())


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
      -0.284018604650239, -0.359061731869329, -54.7330871178434, 5.42628084049087, 7.57918974123273, 6.01442065266234, 4.73611276998701, 1.56199479086, -1.50724459746174, -1.83457550199286,
      -12.9706948486194, -17.9137171346565, -26.3051944262158, 1.39620955663625, 5.47282983218686, 3.30443275331792, 2.44750274824454, 0.427063933399133, -1.41378537195885, -1.66782615655982,
      -19.4850123751347, -29.1229094838281, -29.7445310888168, 11.7953555971751, 24.2220657722924, 13.777282307286, 12.0218140803301, 2.99464178537332, -2.15769087071376, -3.40706808261604

    ),
    
    SE = c(
      1.07890846782321, 1.11404354103527, 5.38462964597645, 0.844350745748173, 1.02057420283281, 0.8714511154634, 0.724914425461319, 0.512736504348169, 0.493456657889419, 0.521089510088924,
      1.16968751620523, 1.69509062565129, 2.36906349944538, 0.444383240799028, 0.617509289698842, 0.528695041838164, 0.432535426629689, 0.359049561822226, 0.35981220770809, 0.365844055107105,
      3.73267694772084, 6.00555563915702, 6.25140025210554, 2.63490328362125, 5.08256352251107, 2.99316925377097, 2.65540643559261, 0.918561299011328, 0.759934775874228, 0.965446997771048
    ),
    Sigma = c(
      24.2774002638299, 35.0217059207632, 36.6084915026449, -2.45708995813618, 8.34900640307237, 2.05665935691602, 5.54327645036427, 0.927051742443414, 5.2778546500144, -9.2708670793003,
      11.4175407175963, 15.3365626464883, -13.8669296446658, 0.387311699424014, 5.84801424289045, 5.24574458450019, -4.87629888282094, -3.4560270345905, 4.14974415590277, 5.22007672279517,
      -56.8969450274998, 61.0136446087724, 53.7126779913724, -3.10321538233686, -19.717468762837, 2.26915281134417, 15.4929865317155, -8.02980252560883, -1.9092108396275, -6.58884101820264
      
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
    filter(!(Group == "Non-smokers" & Population == "Age: 26-64"))
  
  df_Age <- combinaciones %>%
    mutate(
      Mu = c(
        -6.44708867106307, -3.98652442726932, -42.3496055468139, 4.92060857342081, 5.38202473186996, 6.96207404532161, 6.16178290045903, 2.27064495188725, -3.3514910515862, -7.16578702579454,
        -12.4795461936182, -15.1300198041693, -23.8127215839939, 1.91317426330498, 5.26529976269145, 3.22629073329648, 2.100392128781, 0.87105279868963, -2.39906987621858, -3.39484798298336,
        -19.4850123751347, -29.1229094838281, -29.7445310888168, 11.7953555971751, 24.2220657722924, 13.777282307286, 12.0218140803301, 2.99464178537332, -2.15769087071376, -3.40706808261604,
        -0.343481046803741, 2.62235039668198, -54.5904833803419, 5.99902156684161, 6.75872504685144, 6.31366957051404, 3.80881344259453, 0.942186632764606, 0.00593607276930683, 0.829575318691231,
        -14.4817904665056, -20.4981632582736, -31.2801196307919, 1.29819686593095, 5.56904666899233, 3.51674933884077, 2.8356227911218, 0.393368592917362, -0.620429202460959, -0.151398126925698
        
        
      ), 
      SE = c(
        1.82245336685058, 2.02045379971638, 9.06167093575288, 1.74548850267236, 1.98309691235137, 1.82697020226823, 1.60962687325653, 1.14863796226826, 1.1013628939985, 1.57460618040255,
        1.77971352647649, 2.21027795843026, 3.36769451047088, 0.692752475019168, 0.959409929424454, 0.762738671925281, 0.553738488866484, 0.534631592048978, 0.614578097687836, 0.697423204103432,
        3.73267694772084, 6.00555563915702, 6.25140025210554, 2.63490328362125, 5.08256352251107, 2.99316925377097, 2.65540643559261, 0.918561299011328, 0.759934775874228, 0.965446997771048,
        1.33941856611952, 1.69445543998915, 6.8127239746844, 1.12978077206048, 1.20194310055725, 1.14596283386978, 0.837397577340144, 0.62654186441653, 0.591415880748587, 0.594011821383828,
        1.83631017847553, 2.56797460682362, 3.90143684411291, 0.642879841967268, 0.988920656107322, 0.716966425954309, 0.608269070285576, 0.446637871937746, 0.466677666209529, 0.449034046979321
        
      ),
      Sigma = c(
        -19.7622465766796, -31.9737354656102, -2.44487316706552, -7.63582912733986, 13.4229803619121, -0.769860889812069, 8.54513248651111, 7.33704228660296, 6.88844448892098, 11.1427569252978,
        -12.3459302188033, -15.742552497517, 12.3390152600031, 1.76738362299194, 6.23798074347138, 4.68978134362208, -3.10368165285632, 3.19499185467673, -4.59413186435501, -5.9060879537798,
        -56.8969450274998, 61.0136446087724, 53.7126779913724, -3.10321538233686, -19.717468762837, 2.26915281134417, 15.4929865317155, -8.02980252560883, -1.9092108396275, -6.58884101820264,
        22.0459977221046, 33.2422552623445, 34.447527499582, 3.64058007842879, 8.00323150546381, -1.93654334108353, 5.02537970209665, 0.0986529815902948, -5.80722817225859, -8.7055324865389,
        -11.2419644156642, 16.4111313554758, 13.2347771108993, -3.50816455485078, -7.02686866187769, 4.10565507478182, -4.68923915249049, 2.64239007254313, 3.47480883769923, -4.34924969251387
      ) 
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
      Mu = c(
        4.97305946705424, 1.11136294558798, -58.1574443699625, 4.35108584818553, 6.67931605317353, 7.97526671528663, 3.44735640522221, -0.450311817834246, -1.37090068327253, -0.559333213670319,
        -13.0719094287349, -17.5692350839984, -30.2078512719996, 3.1012986154799, 4.90758793228425, 3.89619716726816, 1.87774244454422, 0.292437318066174, -2.53105051218161, -1.12837660806039,
        -34.6804726233814, -29.163777436765, -52.6962844799152, 12.8597263651325, 25.6716955924567, 14.1330684943282, 12.9373110552782, 4.49383564540033, -3.56285697815816, -6.21942569818947,
        -7.38781659637192, 1.18730462372292, -91.1361976323764, 12.2837748990899, 16.0121850592391, 15.1950869187277, 7.37890691354765, 6.27486621894112, -6.32861093813406, -9.26569045947997,
        -16.6509647689175, -23.6094094548931, -43.138938843329, 2.61106556450155, 10.2754378138462, 4.51309051208735, 3.02221135807323, 1.32841053293405, -3.02956984781697, -4.5846765256299,
        -13.9210382525646, -19.6475749418212, -22.6363135466699, 12.1757091693018, 22.1797333824811, 14.435520495077, 10.6998147966194, 0.46074528303419, -2.10751477687571, -3.25057160926604,
        -0.708014868814254, 0.253517215122076, -34.6301643138397, 7.82399655052804, 2.82499515148775, 4.89676850694911, 3.04312131947671, 0.594426418182977, -0.475234734868596, -0.815724502058649,
        -38.6879317032741, -47.496851125256, -48.3659109623532, 2.25506121667223, 6.24165288987871, 4.75843946607999, -2.11563674639159, 3.33911876386532, 2.68475445818545, 0.764347546516625,
        -18.0786900997702, -18.0267745046034, 10.0137431799749, 0.801288895773471, 1.17076747169112, 0.988433161155691, 8.10072380121637, 3.06925332829977, -2.14722570508156, 0.500805765371401
      ), 
      SE = c(
        3.10197086818805, 2.49993267690397, 13.481263421046, 1.93573521050248, 2.17924016867408, 2.29874387978454, 1.54371926651585, 1.11703174304831, 1.40690206624244, 1.15738056920726,
        2.21300050748042, 3.06277317851063, 5.03360797771384, 0.976513404236151, 1.19144762260979, 0.963057614166005, 0.705837290721284, 0.743479374071213, 0.736115509166687, 0.738975657534327,
        14.9337872983648, 12.7220193459974, 22.4681269010031, 6.10912065974388, 11.3277598220717, 6.40182175946216, 6.06692726081316, 2.4848394846195, 2.10439096266332, 3.08812439274512,
        3.19078864777129, 3.24520093940779, 28.900978095736, 4.55301652520161, 5.64824388974238, 5.41552007278802, 2.65651743382352, 2.51611085620356, 2.42788519473299, 3.42298048776244,
        4.13688901005488, 6.20474060814208, 11.0537567926503, 1.40086440289978, 2.91659185755473, 1.86859298216163, 1.29780917874419, 1.03463173595492, 1.26243923830178, 1.53998920875058,
        4.53330909608662, 7.08905599306987, 9.35212516260518, 4.7371940554228, 8.01573269752858, 5.31734489689445, 3.96594485180197, 0.861793824800136, 1.40097645487928, 1.59904266073872,
        1.7505077540935, 1.51148389306896, 5.35147264537724, 1.44649383810023, 1.27762250159372, 1.30811121116214, 1.12308518483156, 0.790799989943137, 0.672281414878168, 0.790799989943137,
        8.37690248597826, 9.95728096453395, 10.0137431799749, 0.801288895773471, 1.17076747169112, 0.988433161155691, 0.775955590605363, 0.500805765371401, 0.578716328667068, 0.500805765371401,
        4.75561230635446, 4.95291481042838, 4.78416344702061, 3.40884658833426, 6.01110990687557, 4.05424447338005, 2.54062597634581, 1.24171330161919, 0.9991647483663, 1.24171330161919
        
      ),
      
      Sigma = c(
        -34.5394082608679, -38.8361078858509, -42.4743170339092, -7.94050559091825, -8.19477686836936, 5.43525490896704, -6.32270207149842, 2.12299549648494, 14.4610448602601, 18.281497984692,
        -13.4297187128579, 16.8670567992996, 16.4569699036693, -2.25854501001175, 6.61436790414093, 1.61892713126285, -3.82872829519259, 6.23788747523229, -7.12716122679326, -8.14851562985396,
        -68.5076643918732, -71.5134886999559, 36.3432745489448, -5.66508762631721, -27.621877933403, -0.351886389781397, -17.1963105906011, 13.7335668948957, 5.16314988583628, -11.8173732844501,
        40.9239394628955, -59.1264977071878, -69.137167313298, -0.10266893607245, -15.1406291184411, 8.88071931592257, 3.50356156921676, 5.7758903224479, -12.0991880003843, -18.5896714565632,
        -15.949734427128, -25.613846383392, 24.0347280307181, 1.9751560222693, -5.66350907245993, -7.95281479586525, 9.43734188258002, -4.32846483493792, -5.02199458570124, 7.58226033347127,
        50.4284446656205, 47.7344975822782, -51.190845571048, -3.10718623039626, -17.4478932630223, -5.34149231985398, -13.0657483961896, -4.60843610232317, 2.73204617334919, -9.0664279360666,
        -26.481907405883, 23.414487095864, 10.2479549996006, 8.14100406182076, 10.1044833124714, 3.25257621977239, -8.58842449270517, -6.54857212059505, -1.80281744817259, -3.29659138367357,
        -13.3824146547806, -5.16301141212525, -23.8896467584706, -2.55340521042456, -7.62019749038298, -2.11563674639159, 2.68475445818545, 1.10569338762576, -0.724179078593528, -3.41041900828749,
        -40.0606364476132, -49.6915578225361, 39.717325073878, 2.5251420094691, 15.4377878597994, 4.32218966811608, 13.4710513729353, -6.71829894807029, 2.28933510286167, 3.87522653853034
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
      Mu = c(
        -1.57376536627553, -1.62487969261922, -59.6747989175575, 6.79727970607292, 8.26038784023568, 7.84638856654869, 5.47439613428213, 1.67016488068368, -2.81185833102677, -4.8931078392412,
        -16.5881289843545, -21.0140563908302, -32.5419435587023, 2.64705917259947, 6.7742177064421, 4.63321273463141, 3.50124789821279, 0.430533752371369, -2.8353343478384, -3.5380159679029,
        -30.5667013810772, -19.1406810972475, -18.2103589797456, 11.3049242325169, 23.3028882213685, 12.6228453358724, 9.73372661801521, 3.01870759726056, -1.72445414606745, -1.68811135876128,
        1.1066426397542, -4.08695883879956, -54.1450062142984, 5.12756149093065, 6.07877147032616, 4.56617914454923, 3.42885741439761, 2.79020144933454, 0.276377899037586, 2.71900501514572,
        -11.7929388759247, -18.5706057756595, -26.4620462032979, 0.936143422160517, 5.02686629081377, 2.81234112243654, 2.13176098838015, 1.04983660346155, 0.471730578705108, 0.0658273736814684,
        -19.80478604446, -35.9161338655641, -0.689229370173456, 11.0668096981927, 22.7312401984483, 14.8072870868653, 11.617779303351, 2.55881723482646, -0.493574290849677, -3.71016104799553

      ), 
      SE = c(
        1.76976316862946, 1.68906764826591, 10.6592185710275, 1.54966626213342, 1.86137005655816, 1.74038354981679, 1.21552564027006, 0.807096747641009, 0.940682280093955, 1.25431135217306,
        2.16349194311274, 2.84323593150132, 4.22836606824876, 0.749421956274442, 1.11657608384691, 0.873066950298281, 0.681635010261013, 0.554918308207686, 0.614399671576252, 0.681641056898271,
        6.87430633769376, 4.53317031477887, 4.92731387094942, 2.81987867545904, 5.68195382569101, 3.15719635322466, 2.45015874340201, 0.993303749161422, 0.840038280373781, 0.821618000647508,
        1.93874998877628, 1.66152753563708, 8.31404164065802, 1.2276361979102, 1.38564273069078, 1.19163099183611, 1.05753420666372, 1.02235465200984, 0.686494192701484, 0.799731481366041,
        1.62125632990647, 2.47010100375128, 3.49331414968906, 0.733704631196419, 0.80037124200366, 0.679892243630576, 0.641950991648366, 0.577578992153379, 0.517302903970467, 0.454991704457266,
        6.25601220700794, 12.2427935418234, 5.9435701085925, 4.43392384680398, 8.16313293026298, 5.43394099750895, 4.29747864318828, 1.42849611382494, 1.39145927169137, 1.74527633620123
), 
      Sigma = c(
        -31.1366098249355, 36.9104925345926, -43.1612135304064, -3.69661677769804, 11.8950100677843, -2.61866701991235, -3.71887145548082, 2.47120799326889, -8.41164404208508, -13.7753242838401,
        -15.548649344429, -17.8061906361364, 12.9550816967818, -2.89520427222273, 7.71705519691524, -5.36323358648942, -4.41345578889145, 4.63978713267346, 5.02547827806513, -6.54856542178076,
        55.8380066837449, -51.7614885637603, -85.1160457615076, 1.16711781310555, 10.833952875281, 1.82044899062083, -11.8683059208726, -5.88697841577366, -2.616244174326, -4.63334596042787,
        -25.9740328672092, 27.981764422496, 29.6274715634857, -0.307795633576577, -5.39437735771201, -3.29415405821128, 5.3829267204191, -3.47772594561232, 5.09491451546195, -6.51932774288872,
        8.69172174084335, -14.5208310012653, 16.6337151791378, 4.72238026196565, 3.1789534413078, -2.06810201561458, -4.41207925479439, 3.96986381622137, 3.5187279855096, 3.99612056682421,
        -66.1913695954007, -82.7696348015185, -107.045203935973, -1.14130651428088, 20.1929954034685, 3.34537285015122, 12.3666281119329, 5.61799829233551, 6.15190298719424, 6.43993957997777
) 
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
plot_dot_error(df_all, grupo = "Traditional smokers", categorias_seleccion = c("Disposable ENDS", "Rechargeable ENDS", "Conventional cigarette"), mostrar_y = 1, limites_x = c(-50, 150) )

plot_alt_TS <- plot_dot_error(df_all, grupo = "Traditional smokers", categorias_seleccion = c("Disposable ENDS", "Rechargeable ENDS", "Conventional cigarette"), mostrar_y = 1, limites_x = c(-50, 150) )
plot_alt_dual <- plot_dot_error(df_all, grupo = "ENDS users-dual", categorias_seleccion = c("Disposable ENDS", "Rechargeable ENDS", "Conventional cigarette"), mostrar_y = 2,  limites_x = c(-50, 150))
plot_alt_NS <- plot_dot_error(df_all, grupo = "Non-smokers", categorias_seleccion = c("Disposable ENDS", "Rechargeable ENDS", "Conventional cigarette"), mostrar_y = 3, limites_x = c(-50, 150) )

(plot_alt_TS | plot_alt_dual | plot_alt_NS) + plot_layout(ncol = 2, nrow = 2)
plot_alternatives <- (plot_alt_TS | plot_alt_dual | plot_alt_NS) + plot_layout(ncol = 2, nrow = 2)


ggsave("Alternatives.png", plot_alternatives, width = 10, height = 14, dpi = 350)
ggsave("Alternatives.svg", plot_alternatives, width = 10, height = 14, dpi = 350,device = "svg")

###########################  Graficos: Atributos  ####################################### 

##### Harm 
plot_harm_TS <- plot_dot_error(df_all, grupo = "Traditional smokers", categorias_seleccion = c("HTO: Mildly Harmful", "HTO: Very Harmful", "HTO: Unknown Harm", "Self Harm: as harmful as cigarette"), mostrar_y = 1, limites_x = c(-50, 25))
plot_harm_dual <- plot_dot_error(df_all, grupo = "ENDS users-dual", categorias_seleccion = c("HTO: Mildly Harmful", "HTO: Very Harmful", "HTO: Unknown Harm", "Self Harm: as harmful as cigarette"), mostrar_y = 2, limites_x = c(-50, 25))
plot_harm_NS <- plot_dot_error(df_all, grupo = "Non-smokers", categorias_seleccion = c("HTO: Mildly Harmful", "HTO: Very Harmful", "HTO: Unknown Harm", "Self Harm: as harmful as cigarette"), mostrar_y = 3, limites_x = c(-50, 25))



(plot_harm_TS | plot_harm_dual | plot_harm_NS) + plot_layout(ncol = 2, nrow = 2)
plot_harm <- (plot_harm_TS | plot_harm_dual | plot_harm_NS) + plot_layout(ncol = 2, nrow = 2)
ggsave("Harm.png", plot_harm, width = 10, height = 14, dpi = 300)
ggsave("Harm.svg", plot_harm, width = 10, height = 14, dpi = 300,device = "svg")

##### Hide
plot_hide_TS <- plot_dot_error(df_all, grupo = "Traditional smokers", categorias_seleccion = c( "Easy to hide" ), mostrar_y = 1, limites_x = c(-20, 20))
plot_hide_dual <- plot_dot_error(df_all, grupo = "ENDS users-dual", categorias_seleccion = c( "Easy to hide" ), mostrar_y = 2, limites_x = c(-20, 20))
plot_hide_NS <- plot_dot_error(df_all, grupo = "Non-smokers", categorias_seleccion = c( "Easy to hide" ), mostrar_y = 3, limites_x = c(-20, 20))

(plot_hide_TS | plot_hide_dual | plot_hide_NS) + plot_layout(ncol = 2, nrow = 2)
plot_hide <- (plot_hide_TS | plot_hide_dual | plot_hide_NS) + plot_layout(ncol = 2, nrow = 2)
ggsave("Hide.png", plot_hide, width = 10, height = 14, dpi = 300)
ggsave("Hide.svg", plot_hide, width = 10, height = 14, dpi = 300,device = "svg")

##### Flavours
plot_flavours_TS <- plot_dot_error(df_all, grupo = "Traditional smokers", categorias_seleccion = c("Menthol", "Fruit or Candy"), mostrar_y = 1, limites_x = c(-25, 25))
plot_flavours_dual <- plot_dot_error(df_all, grupo = "ENDS users-dual", categorias_seleccion = c( "Menthol", "Fruit or Candy"), mostrar_y = 2, limites_x = c(-25, 25))
plot_flavours_NS <- plot_dot_error(df_all, grupo = "Non-smokers", categorias_seleccion = c("Menthol", "Fruit or Candy"), mostrar_y = 3, limites_x = c(-25, 25))


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
  "ASC: disponsable ENDS",         1.586976711,  0.093645743, 1,
  "ASC: disponsable ENDS",         2.135540860,  0.105125256, 0,
  "ASC: rechargeable ENDS",        2.172236825,  0.105633507, 1,
  "ASC: rechargeable ENDS",       -2.811892036,  0.121288636, 0,
  "ASC: conventional cigarette",   5.452181755,  0.213863992, 1,
  "ASC: conventional cigarette",  -4.251531511,  0.172122453, 0,
  
  "Mildly harmful [NEG]",         -0.388427387,  0.055204781, 1,
  "Mildly harmful [NEG]",         -0.590279144,  0.093811539, 0,
  "Very harmful [NEG]",           -0.870561487,  0.062198692, 1,
  "Very harmful [NEG]",           -0.827991124,  0.067572689, 0,
  "Unknown harmful [NEG]",        -0.563304272,  0.053098605, 1,
  "Unknown harmful [NEG]",         0.374537324,  0.126847964, 0,
  "As harmful as cigarette [NEG]",-0.483135022,  0.042810049, 1,
  "As harmful as cigarette [NEG]", 0.602738305,  0.068642693, 0,
  
  "Difficult to hide [NEG]",      -0.138387199,  0.039066966, 1,
  "Difficult to hide [NEG]",      -0.322083636,  0.093586721, 0,
  "Flavor: menthol",               0.174741954,  0.041155976, 1,
  "Flavor: menthol",               0.700066995,  0.062238525, 0,
  "Flavor: fruit or candy",        0.227751790,  0.042562560, 1,
  "Flavor: fruit or candy",       -1.020990926,  0.052150456, 0
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

