# 05_Ch3_Figure_Functions
# Figures for Ch. 3 manuscript

#Explanation:
# --Load from local disk if you have previously loaded from server and saved an .RData of layers.
# --Load from public internet databases and local data files and save the workspace if you do
# not have a local RData of the layers saved.
# -- If you want to update an existing .RData file of layers, simply delete or rename the old one.
# -- Save workspace option provided in case of not wanting to overwrite existing .RData file



# setup -------------------------------------------------------------------

# conversion factors
cfs_to_TAF_per_day = 1/43560 * 60*60*24 / 1000 # 1cfs / sq ft_per_acre * seconds_per_day / 1000
cfs_to_m3day = 1 / 35.3147 * 60*60*24 # ft3/m3 * sec/day
ft_to_m = 1 / 3.28084
Mm3month_30day_to_cfs = 1/30 * 1/(24*60*60) * 10^6 * 35.3147 # 1 period/30 days * 1 day/X seconds * 10^6 m3/Mm3 * 35.3147 ft3/m3
Mm3month_AugDec_to_cfs = 1/30.6 * 1/(24*60*60) * 10^6 * 35.3147 # 1 period/30.6 days * 1 day/X seconds * 10^6 m3/Mm3 * 35.3147 ft3/m3
Mm3month_1day_to_cfs = 1/(24*60*60) * 10^6 * 35.3147 #  1 day/X seconds * 10^6 m3/Mm3 * 35.3147 ft3/m3
mm_to_in = 1/25.4
in_to_mm = 1/mm_to_in

# Set colors
nprc_col = "darkgoldenrod"
bau_col = "black"
precip_col = "mediumpurple"#"royalblue3"
color_state_buffer = "gray"
color_klamath = "khaki"
color_confluence = "deepskyblue"
color_states = "darkolivegreen3"
color_county = "gray40"
color_watershed = "black"
color_watershed_fill = "dodgerblue"
color_interstate = "red"
color_state_road = "brown"
color_river = "blue"
color_cities= "#d7922d"
# color_basin = "black"
color_tribs = "dodgerblue1"
color_gauges = "yellow1"
fall_col = "darkgoldenrod"
spring_col = "green4"
wet_sn_col = "royalblue3"
dry_sn_col = "orangered"
et_col = "darkgoldenrod2"


#Hillshade black-white palette
hillshade_palette_faded = colorRampPalette(c("gray30", "white")) # This washed-out hillshade palette allows legends to be plotted on top
hill_wsh = rast(file.path(scratch_dir, "hillshade_cropped_raster.tif"))

n_sp = 336 #number of stress periods in model run, 2011-2018

# Data processing

#Snow data
snow_cdec$Date = as.Date(snow_cdec$Date, format = "%d-%b-%Y")

# Precip data
wx_stns = noaa_stations$station.id

#read in long-term interp record or generate it
precip_file_path = file.path(local_data_dir,"obs_and_interp_precip_1935_2021.csv")
if(file.exists(precip_file_path)){ppt = read.csv(precip_file_path)}
if(!file.exists(precip_file_path)){
  ppt = get_interp_precip_record()
  write.csv(ppt, file = precip_file_path, row.names = F, quote = F)
}

#Eras and colors. Era 1 is up to 1976; era 2 is 1977-2000; era 3 is 2001-2021.
era_tab=data.frame(era=1:3, 
                   color_minflow = c("firebrick","darkorange","gold"),
                   color_pspill=c("olivedrab3","dodgerblue","darkorchid"))
# Fall flows agg. 
agg_windows = c("aug", "sep", "oct", "nov","dec") # ,"sep_oct", "sep_nov"
ff_color = data.frame(agg_window = agg_windows,
                      # color=brewer.pal(n=length(agg_windows),"Spectral"))
                      color = c("darkred","orangered","goldenrod2","darkolivegreen3","dodgerblue3"))

# color = c("gray40","darkorange4","darkorange2"))

# Wells and WLs

# TEMPORARY UNTIL WE FIX THE LWA PIPELINE
# Add LWA transducer data to wl_obs. Read from local archive
wl_archive = read.csv(file=file.path("C:/Users/Claire/Box/SiskiyouGSP2022_DMS/wl_observations_2021.07.20.csv"))
wl_archive$X = NULL
# lwa_wls = wl_archive[wl_archive$data_table_source == "LWA GWO",]
# wl_obs = rbind(wl_obs, lwa_wls)

wells_archive = read.csv(file = file.path("C:/Users/Claire/Box/SiskiyouGSP2022_DMS/wells_2021.07.20.csv"))
wells_archive$X = NULL
# lwa_wells = wells_archive[wells_archive$well_loc_source=="LWA GWO",]
# wells = rbind(wells, lwa_wells)

# Spatialize wells, subset by watershed, and correct other tables
wells_sp = wells#[wells$basin_name=="Scott River Valley",]
wells_sp = st_as_sf(wells_sp, coords = c("longitude", "latitude"), crs = crs("+init=epsg:4326"))
wells_sp = st_transform(wells_sp, crs("+init=epsg:3310"))
wells_sp = wells_sp[watershed,] # subset for Scott watershed
wells = wells[wells$well_code %in% wells_sp$well_code ,] # subset the table as well
wl_obs = wl_obs[wl_obs$well_code %in% wells_sp$well_code,] # subset the obs. table as well

# Read in and clean ET data. Read from local, since getting it through R is a pain. and the files are small.
et_225 = read.csv(file.path(local_data_dir, "ETo_CIMIS_stn_225.csv"))
et_spatial = read.csv(file.path(local_data_dir, "ETo_spatial_report_aug2019.csv"))

# subset columns and clean
et_225 = et_225[,c("Date", "ETo..mm.")]
et_spatial=et_spatial[,c("Date","ETo..mm.day.")]
colnames(et_225) = c("Date", "et0_mm")
colnames(et_spatial) = c("Date", "et0_mm")
et_225$Date=as.Date(et_225$Date, format = "%m/%d/%Y")
et_225 = et_225[!is.na(et_225$Date),] # take off one NA at the end of the table
et_spatial$Date=as.Date(et_spatial$Date, format = "%m/%d/%Y")
# subset by date
et_spatial = et_spatial[et_spatial$Date < min(et_225$Date) & 
                          et_spatial$Date >= as.Date("2003-10-01"),]

et_0 = rbind(et_spatial, et_225)

# Stream line data processing
# Isolate river
riv = named_streams[named_streams$gnis_name == "Scott River", ]
# Reduce number of streams to major tributaries.
mapped_stream_names = c("Shackleford Creek","Mill Creek", "Oro Fino Creek", 
                        "Moffett Creek", "Kidder Creek", "Patterson Creek",
                        "Crystal Creek","Johnson Creek", "Etna Creek",
                        "Clark Creek", "French Creek", "Miners Creek", "Sugar Creek",
                        "Scott River", "East Fork Scott River", 
                        "South Fork Scott River",
                        "Wildcat Creek")
mapped_streams = named_streams[named_streams$gnis_name %in% mapped_stream_names,]
#clean up mill and patterson creeks - there are multiple creeks with that name and we just want the major tributaries
patterson_reaches = 18010208000000 + c(203, 204, 205, 1691)
patterson_reaches_exclude = 18010208000000 + c(237, 583, 2579, 2622, 2639)
mill_reaches = 18010208000000 + c(580, 1888, 7481, 58, 59, 60)
mill_exclude_reaches = 18010208000000 + c(157, 158, 159, 160, 161, 162, 163, 164, 165, 349, 350, 351, 352)
eliminate_these_reaches = (mapped_streams$gnis_name == "Mill Creek" & !(mapped_streams$reachcode %in% mill_reaches)) | 
  (mapped_streams$gnis_name == "Patterson Creek" 
   & !(mapped_streams$reachcode %in% patterson_reaches)) 
mapped_streams = mapped_streams[!eliminate_these_reaches,]

# subfunctions ------------------------------------------------------------

wtr_yr = function(date_vector){
  water_year_vector = year(date_vector)
  water_year_vector[month(date_vector) >9] = year(date_vector[month(date_vector) >9]) + 1
  return(water_year_vector)
}

get_sfr_data = function(scenario_id = "basecase", 
                          start_date = as.Date("1990-10-01"), end_date = as.Date("2018-09-30"),
                          start_wy = 1991, end_wy = 2018){
  
  
  # 3. Specify file location
  sfr_results_file_path = file.path(svihm_results_dir, 
                                paste(scenario_id, "SFR results.csv"))
  
  # 1. look for files on local disk. if found, read csv.
  if(file.exists(sfr_results_file_path)){
    # sfr_results = readRDS(file = sfr_results_file_path)
    # FJ_Outflow$Date = as.Date(FJ_Outflow$Date)
    sfr_tab = read.csv(file = sfr_results_file_path)
    
  }
  
  # 2. if not, need to digest files. look for scenario dir on local disk
  if(!file.exists(sfr_results_file_path)){
    scenario_dir = file.path(svihm_scenarios_dir, scenario_id)
    # If not there, check hard drive
    if(!file.exists(file.path(scenario_dir,'Streamflow_Global.dat'))){
      scenario_dir = file.path(svihm_scenarios_backup_dir, scenario_id)
    }
    
    #3. Digest SFR budgets from scenario directory and save to local drive
    sfr_glob_text = readLines(file.path(scenario_dir, "Streamflow_Global.dat"))
    start_rows = grep("STREAM LISTING", sfr_glob_text) + 5 #one start for each stress period
    n_reach = start_rows[2]-start_rows[1]-8  # 8 extra header rows at each timestep
    
    colname_list = c("LAYER", "ROW","COL", "STREAM_SEG_NO", "RCH_NO", "FLOW_INTO_STRM_RCH",
                     "FLOW_TO_AQUIFER", "FLOW_OUT_OF_STRM_RCH","OVRLND_RUNOFF","DIRECT PRECIP",
                     "STREAM_ET","STREAM_HEAD", "STREAM_DEPTH", "STREAM_WIDTH",
                     "STREAMBED_CONDCTNC","STREAMBED_GRADIENT")
    ncols = length(colname_list)
    n_sp = length(start_rows) # length(seq.Date(from = start_date, to = end_date, by = "month")) 
    
    # store results in an array or a dataframe
    # reach_array = array(data=NA, dim = c(n_sp, n_reach, ncols))
    reach_df = data.frame(matrix(data = NA, nrow = n_reach * n_sp, ncol = ncols+1))
    colnames(reach_df) = c("STRESS_PERIOD",colname_list)
    reach_df$STRESS_PERIOD = sort(rep(1:n_sp, n_reach))
    
    for(i in 1:n_sp){
      start_row = start_rows[i];
      sfr_stress = sfr_glob_text[start_row:(start_row+n_reach-1)]
      
      sfr_reaches = data.frame(matrix(data = NA, nrow = n_reach, ncol = ncols))
      colnames(sfr_reaches) = colname_list
      for(j in 1:n_reach){
        sfr_reach = unlist(strsplit(trimws(sfr_stress[j]), " ")) #split on space character
        sfr_reach = sfr_reach[nchar(sfr_reach)>0] #this produces a lot of blank strings; get rid of those
        # reach_array[i,j,] = sfr_reach
        sfr_reaches[j,] = sfr_reach
      }
      reach_df_indices = reach_df$STRESS_PERIOD == i
      reach_df[reach_df_indices,2:(ncols+1)] = sfr_reaches
    }
    # store dataframe
    sfr_tab = reach_df
    # saveRDS(object = reach_array, file = sfr_results_file_path)
    write.csv(x = reach_df, file = sfr_results_file_path, quote = F, row.names = F)
  }

  return(sfr_tab)
}


get_simulated_fj_outflow = function(scenario_id = "basecase", 
                                    start_date = as.Date("1990-10-01"), end_date = as.Date("2018-09-30"),
                                    start_wy = 1991, end_wy = 2018){
  
  # 3. if not found, look for scenario dir on external drive
  fj_flow_file_path = file.path(svihm_results_dir, 
                                paste(scenario_id, "FJ outflow.csv"))
  
  # 1. look for files on local disk. if found, read csv.
  if(file.exists(fj_flow_file_path)){
    FJ_Outflow = read.csv(fj_flow_file_path)
    FJ_Outflow$Date = as.Date(FJ_Outflow$Date)
  }
  
  # 2. if not, need to digest files. look for scenario dir on local disk
  if(!file.exists(fj_flow_file_path)){
    scenario_dir = file.path(svihm_scenarios_dir, scenario_id)
    if(!file.exists(file.path(scenario_dir,'Streamflow_FJ_SVIHM.dat'))){
      scenario_dir = file.path(svihm_scenarios_backup_dir, scenario_id)
    }
    
    #3. Digest water budgets from scenario directory and save to local drive
    
    FJ_Outflow = read.table(file.path(scenario_dir,'Streamflow_FJ_SVIHM.dat'), skip = 2)[,3]
    cfs_to_m3d = 1/35.3147 * 86400 # 1 m3/x ft3 * x seconds/day
    FJ_Outflow = data.frame(Date = seq(start_date, end_date, by = 'day'), 
                            fj_flow_m3d = FJ_Outflow,
                            Flow = FJ_Outflow / cfs_to_m3d)
    # add water year
    FJ_Outflow$wy = year(FJ_Outflow$Date)
    FJ_Outflow$wy[month(FJ_Outflow$Date) > 9] = 
      year(FJ_Outflow$Date[month(FJ_Outflow$Date) > 9]) + 1
    
    write.csv(x = FJ_Outflow, file = fj_flow_file_path, quote = F, row.names = F)
  }
  return(FJ_Outflow)
}

get_aq_stream_flux_summary_tab = function(scenario_id = "basecase", show_plots = F){
  sfr_tab = get_sfr_data(scenario_id = scenario_id)
  
  # summarize SFR aquifer exchange data
  # negative numbers in flow_to_aquifer means a gaining reach.
  # positive means a losing reach.
  tot_discharge_to_stm = aggregate(x = sfr_tab$FLOW_TO_AQUIFER,
                                   by = list(sfr_tab$STRESS_PERIOD),
                                   function(x){sum(x[x<0])*-1})
  tot_flow_infiltrating = aggregate(x = sfr_tab$FLOW_TO_AQUIFER,
                                    by = list(sfr_tab$STRESS_PERIOD),
                                    function(x){sum(x[x>0])}) # keep this number positive
  net_discharge_to_stm = aggregate(x = sfr_tab$FLOW_TO_AQUIFER,
                                   by = list(sfr_tab$STRESS_PERIOD),
                                   function(x){sum(x)*-1})
  fj_bc = get_simulated_fj_outflow(scenario_id = "basecase")
  fj_bc$month_day1 = floor_date(fj_bc$Date, unit = "month")
  fj_sp = aggregate(fj_bc$FJ_Flow_m3d, by = list (fj_bc$month_day1), FUN = sum)
  
  # summary tab
  n_sp = 336
  summary_tab = data.frame(SP = 1:n_sp, 
                           tot_flow_to_stm = tot_discharge_to_stm$x / 10^6,
                           tot_flow_to_aq = tot_flow_infiltrating$x / 10^6,
                           net_flow_to_stm =  net_discharge_to_stm$x / 10^6)
  # add FJ flow for basecase and NPRC?
  summary_tab$fj_Mm3= fj_sp$x / 10^6
  
  # 3D plot of baseflow discharge, infiltration (stream leakage), and FJ flow
  # plot3d(x = summary_tab$tot_flow_to_stm, 
  #        y = summary_tab$tot_flow_to_aq, 
  #        z = summary_tab$fj_m3,
  #        xlab = "Baseflow discharge", ylab = "Stream Leakage", "FJ Flow")
  
  # if(show_plots == T){
  # }
  
  return(summary_tab)
  
}

convert_date_to_SP = function(target_date = as.Date("2000-01-15"),
                              start_date = as.Date("1990-10-01"),
                              end_date=as.Date("2018-09-30")){
  months_day1 = seq.Date(from = start_date, to = end_date, by = "month")  # first day of each SP
  target_date_monthday1 = floor_date(target_date, unit = "months") # first day of the month it is in
  SP = which(months_day1 == target_date_monthday1) # match
  
  return(SP)
}


# Table functions -------------------------
get_sim_and_obs_gradients_tab = function(days_window = 7, 
                                         get_matching_sim_grads = T,
                                         start_date = as.Date("1990-10-01"), 
                                         end_date = as.Date("2018-09-30")){
  H = modflow_heads_bc
  n_sp = length(H)#336
  names(H) = paste0("SP",1:n_sp)
  layer = 1
  x_res = xres(svihm_raster)
  y_res = yres(svihm_raster)
  # sp= 1
  # h_lay1_sp = H$SP1[,,layer] # Keep only layer 1 heads. 
  
  # for imaging / checking values
  # h_lay1_sp_t = t(H$SP1[,,layer]) # Keep only layer 1 heads. 
  # ^ Transpose for visualising in R because it's stored as lat-long instead of x-y
  # n_row = nrow(h_lay1_sp_t) #210. 
  # n_col = ncol(h_lay1_sp_t) #440
  # h_lay1_sp_t = h_lay1_sp_t[,rev(1:n_col)] 
  # # ^ reverse y-direction because it got transposed.
  # # latitude does not need to be reversed, since it goes right to left to start with.
  # 
  # matrix1 = apply(X=h_lay1_sp, MARGIN = 2, FUN = c) # convert to matrix
  # image(matrix1)
  
  # assign matrix values to a raster, for spatial computations
  # gse = svihm_raster; values(gse) = ground_elev
  # wl = svihm_raster; values(wl) = h_lay1_sp
  # plot(wl) # test it
  
  # find places to extract gradients from
  
  tail_up = "Q21"
  tail_down = "E3"
  # tailgrad = (wl_tailup - wl_taildown)/ distance(tail_up, tail_down)  #Tailings reach gradient to river
  
  fre_up = c("W10")
  fre_mid = c("N17", "SCT_178")
  fre_down = "D40"
  
  etn_up =c("Z36", "R18", "L31") # . Horn (south etna fan): 42N09W27N002M, 42N09W27N001M
  etn_down = c("C12", "D16")
  
  etg_up = c("F56", "B9", "E71", "A41")
  etg_down = c("H6", "I16", "G11")
  
  gvw_up = c("C26", "L32", "B9", "B3")
  gvw_down = c("415644N1228541W001","O32") # DWR_4 (43N09W23F001M)
  
  # CURRENTLY HERE. build these lists.
  
  ham_up = c("M2", "P18")
  ham_down = c("415635N1228315W001", "S19", "C11")
  
  mof_up = c("416288N1228303W001")#. further up: N15, G40, P23, A4
  mof_down = c("X7", "416033N1228528W001", "K12", "SCT_197")
  
  qvi_up = c("QV16", "QV15", "QV03", "QV18", "QV04")
  qvi_down = c("QV01", "QV02")
  
  # srr_up: DWR_3 (44N09W28P001M), F6
  # srr_down: fj flow/stage?
  
  gradient_areas = c("tail", "fre", "etn", "etg", "gvw", "ham", "mof", "qvi")
  grad_wells_up = list(tail = tail_up, fre = fre_up, etn = etn_up,
                       etg = etg_up, gvw = gvw_up, ham = ham_up, 
                       mof = mof_up, qvi = qvi_up)
  grad_wells_down = list(tail = tail_down, fre = fre_down, etn = etn_down,
                         etg = etg_down, gvw = gvw_down, ham = ham_down, 
                         mof = mof_down, qvi = qvi_down)
  grad_tab_all = data.frame(grad_areas =  vector(mode = "character", length=0),
                            grad_well_up = vector(mode = "character", length=0),
                            grad_well_down = vector(mode = "character", length=0),
                            elev_up = vector(mode = "numeric",length=0),
                            elev_down = vector(mode = "numeric",length=0),
                            date_up = vector(mode = "character", length=0),
                            date_down = vector(mode = "character", length=0),
                            dist_up_down_m = vector(mode = "numeric",length=0),
                            gradient = vector(mode = "numeric",length=0))
  if(get_matching_sim_grads == T){
    grad_tab_all$elev_up_sim = vector(mode = "numeric",length=0)
    grad_tab_all$elev_down_sim = vector(mode = "numeric",length=0)
    grad_tab_all$sim_SP =vector(mode = "numeric",length=0)
    grad_tab_all$dist_sim_m =vector(mode = "numeric",length=0)
    grad_tab_all$gradient_sim =vector(mode = "numeric",length=0)
  } # add sim columns
  
  for(i in 1:length(gradient_areas)){
    grad_area = gradient_areas[i]
    ups = grad_wells_up[[grad_area]]
    downs = grad_wells_down[[grad_area]]
    wl_downs = wl_obs[wl_obs$well_code %in% downs,]
    # check every up-well 
    for(j in 1:length(ups)){
      up_well_id = ups[j]
      wl_up_well = wl_obs[wl_obs$well_code== up_well_id,]
      # check every data point of the up WL record
      for(k in 1:nrow(wl_up_well)){
        # if there is a down-wl observation within the designated window, 
        up_obs = wl_up_well[k,]
        possible_grad_selector = abs(wl_downs$date - up_obs$date) <= days_window &
          !is.na(wl_downs$date - up_obs$date) 
        n_grads = sum(possible_grad_selector)
        # print(paste(k,",",n_grads)) # diagnostic to find multi-grad cases
        if( n_grads > 0){
          #calculate the gradient(s) for that observation
          downs_selected = wl_downs[possible_grad_selector,]
          # generate sets of points for calculating the distance between the gradient wells
          ups_sp = wells_sp[rep(which(wells_sp$well_code==up_well_id),n_grads),]
          downs_sp = wells_sp[match(downs_selected$well_code,wells_sp$well_code),]
          # define grad_tab for the gradients associated with this up-observation
          grad_tab = data.frame(grad_areas = grad_area,
                                grad_well_up = rep(up_well_id, n_grads),
                                grad_well_down = downs_selected$well_code,
                                elev_up = rep(up_obs$wse_ft, n_grads),
                                elev_down = downs_selected$wse_ft,
                                date_up = rep(up_obs$date, n_grads),
                                date_down = downs_selected$date,
                                dist_up_down_m = pointDistance(p1 = ups_sp, p2 = downs_sp),
                                gradient = NA)
          if(get_matching_sim_grads == T & 
             up_obs$date >= start_date & up_obs$date <= end_date){ # if in the model period
            # Get simulated gradients also
            SP = convert_date_to_SP(target_date = up_obs$date)
            sp_name = paste0("SP",SP)
            H_sp = H[[sp_name]][,,layer] # keep layer 1
            H_sp_raster = svihm_raster; values(H_sp_raster)=H_sp
            
            # two ways to get distances. If they're in hob, get the cell distances
            if(sum(hob_info$well_code==up_obs$well_code)>0){
              up_row = hob_info$ROW[hob_info$well_code==up_obs$well_code]
              up_col = hob_info$COLUMN[hob_info$well_code==up_obs$well_code]
              down_rows = hob_info$ROW[match(downs_selected$well_code,hob_info$well_code)]
              down_cols = hob_info$COLUMN[match(downs_selected$well_code,hob_info$well_code)]
              grad_distances = sqrt((abs(up_row - down_rows) * x_res)^2 + 
                                      (abs(up_col - down_cols) * y_res)^2)
              
              elevs_up = rep(H_sp[up_row,up_col], n_grads)
              elevs_down = H_sp[as.matrix(cbind(down_rows,down_cols))]
            }
            if(sum(hob_info$well_code==up_obs$well_code)==0){
              # if not, get them from point distances from wells_sp
              grad_distances = pointDistance(p1 = ups_sp, p2 = downs_sp)
              elevs_up = extract(x = H_sp_raster, y = ups_sp)
              elevs_down = extract(x = H_sp_raster, y = downs_sp)
            }
            
            grad_tab$elev_up_sim = elevs_up
            grad_tab$elev_down_sim = elevs_down
            grad_tab$sim_SP = rep(SP,n_grads)
            grad_tab$dist_sim_m = grad_distances
            grad_tab$gradient_sim = (grad_tab$elev_up_sim - grad_tab$elev_down_sim) / 
              grad_tab$dist_sim_m
          }
          if(get_matching_sim_grads == T & 
             !(up_obs$date >= start_date & up_obs$date <= end_date)){
            # If we want the sim gradients, but the date's outside the model range, just add NAs
            grad_tab$elev_up_sim = NA
            grad_tab$elev_down_sim = NA
            grad_tab$sim_SP = NA
            grad_tab$dist_sim_m = NA
            grad_tab$gradient_sim = NA
          }
          
          grad_tab_all = rbind(grad_tab_all, grad_tab) # append rows
        } # end grad calc 
      } # k loop
    } # j loop
  } #  i loop 
  
  grad_tab_all$gradient = (grad_tab_all$elev_up - grad_tab_all$elev_down) * ft_to_m / 
    grad_tab_all$dist_up_down_m
  
  return(grad_tab_all)
}


get_dsb_tab=function(start_yr = 1942, end_yr = 2021,
                           too_many_precip_nas = 5){
  fj_flow$Flow_m3day = fj_flow$Flow * cfs_to_m3day
  fj_flow$year = year(fj_flow$Date)
  
  date1=as.Date(paste(start_yr, "01","01",sep="-"))
  date2=as.Date(paste(end_yr, 12,"31",sep="-"))
  fj_ff = fj_flow[fj_flow$Date >= date1 & fj_flow$Date <= date2,]
  
  # 1. Aggregate fall flows by various windows
  
  #make selectors
  jan = month(fj_ff$Date)==1
  feb = month(fj_ff$Date)==2
  mar = month(fj_ff$Date)==3
  apr = month(fj_ff$Date)==4
  may = month(fj_ff$Date)==5
  jun = month(fj_ff$Date)==6
  jul = month(fj_ff$Date)==7
  aug = month(fj_ff$Date)==8
  sep = month(fj_ff$Date)==9
  oct = month(fj_ff$Date)==10
  nov = month(fj_ff$Date)==11
  dec = month(fj_ff$Date)==12
  sep_oct = month(fj_ff$Date) %in% 9:10
  sep_nov = month(fj_ff$Date) %in% 9:11
  
  output = data.frame(year = start_yr:end_yr,
                      era = NA,
                      jan_flow = aggregate(fj_ff$Flow_m3day[jan],
                                           by = list(fj_ff$year[jan]),
                                           FUN = sum)$x,
                      feb_flow = aggregate(fj_ff$Flow_m3day[feb],
                                           by = list(fj_ff$year[feb]),
                                           FUN = sum)$x,
                      mar_flow = aggregate(fj_ff$Flow_m3day[mar],
                                           by = list(fj_ff$year[mar]),
                                           FUN = sum)$x,
                      apr_flow = aggregate(fj_ff$Flow_m3day[apr],
                                           by = list(fj_ff$year[apr]),
                                           FUN = sum)$x,
                      may_flow = aggregate(fj_ff$Flow_m3day[may],
                                           by = list(fj_ff$year[may]),
                                           FUN = sum)$x,
                      jun_flow = aggregate(fj_ff$Flow_m3day[jun],
                                           by = list(fj_ff$year[jun]),
                                           FUN = sum)$x,
                      jul_flow = aggregate(fj_ff$Flow_m3day[jul],
                                           by = list(fj_ff$year[jul]),
                                           FUN = sum)$x,
                      aug_flow = aggregate(fj_ff$Flow_m3day[aug],
                                           by = list(fj_ff$year[aug]),
                                           FUN = sum)$x,
                      sep_flow = aggregate(fj_ff$Flow_m3day[sep],
                                           by = list(fj_ff$year[sep]),
                                           FUN = sum)$x,
                      oct_flow = aggregate(fj_ff$Flow_m3day[oct],
                                           by = list(fj_ff$year[oct]),
                                           FUN = sum)$x,
                      nov_flow = aggregate(fj_ff$Flow_m3day[nov],
                                           by = list(fj_ff$year[nov]),
                                           FUN = sum)$x,
                      dec_flow = aggregate(fj_ff$Flow_m3day[dec],
                                           by = list(fj_ff$year[dec]),
                                           FUN = sum)$x,
                      sep_oct_flow = aggregate(fj_ff$Flow_m3day[sep_oct],
                                               by = list(fj_ff$year[sep_oct]),
                                               FUN = sum)$x,
                      sep_nov_flow = aggregate(fj_ff$Flow_m3day[sep_nov],
                                               by = list(fj_ff$year[sep_nov]),
                                               FUN = sum)$x)
  # add era info to fall flow table. Based on long term hydrograph.
  output$era = 1
  output$era[output$year %in% 1977:2000] = 2
  output$era[output$year %in% 2001:2021] = 3
  
  # 2. Add snowpack depth and timing details.
  
  # add water year.
  # We are using water year to subset the snow data and 
  # year to subset the fall flows data, to accurately
  # reflect that any snow from, say, Oct-Apr can affect the
  # following Sep-Dec
  snow_cdec$water_year = wtr_yr(snow_cdec$Date) 
  
  for(i in 1:length(snow_stns)){
    stn = snow_stns[i]
    # colname1 = paste0(stn,"_max_wc_in")
    colname1 = paste0(stn,"_max_wc_mm")
    # colname2 = paste0(stn,"_month_max")
    colname2 = paste0(stn,"_jday_of_max")
    
    output[,colname1] = NA
    output[,colname2] = NA
    
    for(j in 1:nrow(output)){
      year = output$year[j]
      
      snow_meas_that_year = snow_cdec[snow_cdec$water_year == year &
                                        snow_cdec$Station_code == stn,]
      if(nrow(snow_meas_that_year)>0){
        output[j,colname1] = max(snow_meas_that_year$Water_Content_in * in_to_mm)
        # output[j,colname2] = month(snow_meas_that_year$Date[
        #   which.max(snow_meas_that_year$Water_Content_in)])
        date_of_max = snow_meas_that_year$Date[which.max(snow_meas_that_year$Water_Content_in)]
        output[j,colname2] = as.numeric(format(date_of_max,"%j"))
      }
    }
  }
  
  # 3. Add precip data

  for(i in 1:length(wx_stns)){
    stn = wx_stns[i]
    colname1 = paste0(stn,"_oct_apr_mm")
    # colname2 = paste0(stn,"_month_max")
    colname2 = paste0(stn,"_jday_of_median")
    
    output[,colname1] = NA
    output[,colname2] = NA
    
    for(j in 1:nrow(output)){
      year = output$year[j]
      wat_yr_d1 = as.Date(paste(year-1,"10","01", sep = "-"))
      apr_end = as.Date(paste(year,"04","30", sep = "-"))
      
      stn_meas_that_year = noaa[noaa$DATE >= wat_yr_d1 & 
                                  noaa$DATE <= apr_end &
                                        noaa$STATION == stn,]
      if(nrow(stn_meas_that_year)>0){
        # fill NAs with 0s. to let cumsum function.
        smty = stn_meas_that_year # shorten name
        if(sum(is.na(smty$PRCP) < too_many_precip_nas)){
          smty$PRCP[is.na(smty$PRCP)] = 0
          
          oct_apr_ppt = sum(smty$PRCP, na.rm=T)
          
          output[j,colname1] = oct_apr_ppt
          # output[j,colname2] = month(snow_meas_that_year$Date[
          #   which.max(snow_meas_that_year$Water_Content_in)])
          index_of_median = min(which(cumsum(smty$PRCP) > 
                                         (oct_apr_ppt / 2))) 
          output[j,colname2] = index_of_median # index here = days since Sep 30, or julian day in water year
          
        }
      }
    }
  }
  
  # 3.5 Add the precip data for the interpolated precip record.
  interp_precip = read.csv(file.path(local_data_dir, "obs_and_interp_precip_1935_2021.csv"))
  interp_precip = interp_precip[,c("Date","interp_cal_fj_mean")]
  
  stn = "interp_cal_fj"
  colname1 = paste0(stn,"_oct_apr_mm")
  # colname2 = paste0(stn,"_month_max")
  colname2 = paste0(stn,"_jday_of_median")
  
  output[,colname1] = NA
  output[,colname2] = NA
  
  for(j in 1:nrow(output)){
    year = output$year[j]
    wat_yr_d1 = as.Date(paste(year-1,"10","01", sep = "-"))
    apr_end = as.Date(paste(year,"04","30", sep = "-"))
    
    stn_meas_that_year = interp_precip[interp_precip$Date >= wat_yr_d1 & 
                                         interp_precip$Date <= apr_end,]
    if(nrow(stn_meas_that_year)>0){
      # fill NAs with 0s. to let cumsum function.
      smty = stn_meas_that_year # shorten name
      if(sum(is.na(smty$PRCP)) < too_many_precip_nas){
        smty$interp_cal_fj_mean[is.na(smty$interp_cal_fj_mean)] = 0
        oct_apr_ppt = sum(smty$interp_cal_fj_mean, na.rm=T)
        output[j,colname1] = oct_apr_ppt
        # output[j,colname2] = month(snow_meas_that_year$Date[
        #   which.max(snow_meas_that_year$Water_Content_in)])
        index_of_median = min(which(cumsum(smty$interp_cal_fj_mean) > 
                                      (oct_apr_ppt / 2))) 
        output[j,colname2] = index_of_median # index here = days since Sep 30, or julian day in water year
        
      }
    }
  }
  
  #4. Add ET data.
  
  et_0$water_year = wtr_yr(et_0$Date)
  et_0_oct_apr = et_0[!(month(et_0$Date) %in% 5:9),]
  
  et_agg = aggregate(et_0_oct_apr$et0_mm, by = list(et_0_oct_apr$water_year), FUN = sum)
  colnames(et_agg) = c("water_year","oct_apr_et0_mm")
  
  output$et0_oct_apr = et_agg$oct_apr_et0_mm[match(output$year, et_agg$water_year)]
  
  # 5. Add WLs. (see other function)
  
  return(output)
}

add_wls_to_fall_flows = function(dsb_tab, fft_well_ids){
  fft = dsb_tab
  n_wells = length(fft_well_ids)
  n_years = nrow(fft)
  ncol_fft_init = ncol(fft)
  # Initialize table with places for april WLs from all the highlighted wells
  wl_cols = data.frame(matrix(data=NA, nrow = n_years, ncol = n_wells*2))
  colnames(wl_cols) = c(paste0("aprWL_",fft_well_ids), paste0("springWL_",fft_well_ids))
  wl_cols$year=fft$year
  # Attach columns to fft
  fft = merge(fft, wl_cols, by="year")
  
  for(j in 1:n_wells){
    apr_col_index = ncol_fft_init + j
    spring_col_index = ncol_fft_init + n_wells + j
    well_id=unlist(strsplit(colnames(fft)[apr_col_index], split = "aprWL_"))[2]
    apr_wls = wl_obs[wl_obs$well_code == well_id & month(wl_obs$date) == 4,
                     c("date","wse_ft")]
    spring_wls =  wl_obs[wl_obs$well_code == well_id & month(wl_obs$date) %in% 3:5,
                         c("date","wse_ft")]
    # if there's multiple WLs in the same year, average them. 
    apr_wl_by_yr = aggregate(apr_wls$wse_ft, by = list(year(apr_wls$date)), FUN = mean, na.rm=T)
    spring_wl_by_yr = aggregate(spring_wls$wse_ft, by = list(year(spring_wls$date)), FUN = mean, na.rm=T)
    colnames(apr_wl_by_yr) = c("year", "wl_ftamsl")
    colnames(spring_wl_by_yr) = c("year", "wl_ftamsl")
    
    fft[,apr_col_index] = apr_wl_by_yr$wl_ftamsl[match(fft$year, apr_wl_by_yr$year)]
    fft[,spring_col_index] = spring_wl_by_yr$wl_ftamsl[match(fft$year, spring_wl_by_yr$year)]
  }
  return(fft)
}



get_time_cumprecip_flow_tab = function(wys = 1944:2021,save_pdf=F){
  fj_flow$Flow_m3day = fj_flow$Flow * cfs_to_m3day
  # first task: precip munging
  fj_flow$flow_diff = c(NA,diff(fj_flow$Flow))
  # Subset FJ flow for this analysis
  fj3d = fj_flow[month(fj_flow$Date) %in% c(9:12,1:3),]
  # assign Sept to following WY
  fj3d$wy_3d = fj3d$wy
  fj3d$wy_3d[month(fj3d$Date)==9] = fj3d$wy[month(fj3d$Date)==9] + 1
  # restrict to just complete WYs
  fj3d = fj3d[fj3d$wy_3d %in% wys,]
  # initialize columns for cumulative precip and modified day of WY (days after aug31)
  fj3d$cum_ppt = NA
  fj3d$days_after_aug31 = NA
  
  
  # for each water year, add cumulative rainfall and days in wy columns
  for(i in 1:length(wys)){
    wy = wys[i]
    # print(wy) # damn, this takes forever. why the fuck is that?
    # add days in wy (starting sep 1)
    fj3d_indices = fj3d$wy_3d==wy
    wy_day1 =  as.Date(paste0(wy-1, "-09-01"))
    fj3d$days_after_aug31[fj3d_indices] = as.numeric(fj3d$Date[fj3d_indices] - wy_day1 + 1)
    # calculate cum precip
    prcp_day1 = as.Date(paste0(wy-1,"-09-01"))
    prcp_day_last = as.Date(paste0(wy,"-03-31"))
    ppt_i = ppt[ppt$Date>=prcp_day1 & ppt$Date<=prcp_day_last,c("Date","interp_cal_fj_mean")]
    fj3d$ppt[fj3d_indices] = ppt_i$interp_cal_fj_mean
    fj3d$cum_ppt[fj3d_indices] = cumsum(ppt_i$interp_cal_fj_mean)
    #Calculate flow rate of change (cfs/day)
    # fj3d$d_flow[fj3d_indices]
    fj3d$cum_flow_m3[fj3d_indices] = cumsum(fj3d$Flow_m3day[fj3d_indices])
  }
  
  if(save_pdf ==T){
    # plot all WY days vs cumulative precip
    pdf(file="days vs cum.precip vs flow.pdf", width = 8.5, height = 11/2)
    
    plot(x = fj3d$days_after_aug31, y = fj3d$cum_ppt, pch = 18, col = rgb(.5,.5,.1,.1),
         xlab = "Day of WY (starting Sept. 1)", ylab = "Cum. Precip., FJ Ranger Stn (mm)")
    grid()
    
    # plot WY days vs cumulative precip (1 line per year)
    for(i in 1:length(wys)){
      wy = wys[i]
      if(i==1){
        plot(x = range(fj3d$days_after_aug31), 
             y = range(fj3d$cum_ppt, na.rm=T), pch = 18, col = NA,
             xlab = "Day of WY (starting Sept. 1)", ylab = "Cumulative Precip., FJ Ranger Stn (mm)")
        grid()
      } else {
        fj3d_indices = fj3d$wy_3d==wy
        lines(x = fj3d$days_after_aug31[fj3d_indices], 
              y = fj3d$cum_ppt[fj3d_indices], col = rgb(.5,.5,.1,.3))
      }
    }
    
    plot(x = fj3d$days_after_aug31, y = fj3d$Flow, pch = 18, col = rgb(.5,.1,.5,.1), log = "y",
         xlab = "Day of WY (starting Sept. 1)", ylab = "FJ Flow (cfs)")
    grid()
    # plot WY days vs flow (1 line per year)
    for(i in 1:length(wys)){
      wy = wys[i]
      if(i==1){
        plot(x = range(fj3d$days_after_aug31), 
             y = range(fj3d$Flow, na.rm=T), pch = 18, col = NA, log = "y",
             xlab = "Day of WY (starting Sept. 1)", ylab = "FJ Flow (cfs)")
        grid()
      } else {
        fj3d_indices = fj3d$wy_3d==wy
        lines(x = fj3d$days_after_aug31[fj3d_indices], 
              y = fj3d$Flow[fj3d_indices], col = rgb(.5,.1,.5,.3))
      }
    }
    
    plot(x = fj3d$cum_ppt, y = fj3d$Flow, pch = 18, col = rgb(.1,.5,.5,.1), #ylim = c(0,1000),
         log = "y",
         xlab = "Cumulative Precip., FJ Ranger Stn (mm)", ylab = "FJ Flow (cfs)")
    grid()
    # lm_flow_cppt = lm(fj3d$Flow~fj3d$cum_ppt)
    # abline(lm_flow_cppt, lty = 2)
    
    # plot cum precip vs flow (1 line per year)
    for(i in 1:length(wys)){
      wy = wys[i]
      if(i==1){
        plot(x = range(fj3d$cum_ppt,na.rm=T), 
             y = range(fj3d$Flow, na.rm=T), pch = 18, col = NA, log = "y",
             xlab = "Cumulative Precip., FJ Ranger Stn (mm)", ylab = "FJ Flow (cfs)")
        grid()
      } else {
        fj3d_indices = fj3d$wy_3d==wy
        lines(x = fj3d$cum_ppt[fj3d_indices], 
              y = fj3d$Flow[fj3d_indices], col = rgb(.1,.5,.5,.3))
      }
    }
    # annotate with lag calc point
    # points(x=P_max, y=flow_threshold)
    
    dev.off()
  }
  return(fj3d)
}

add_interp_fall_rains_to_fft = function(dsb_tab,
                                        fall_rain_start_day = "09-01",
                                        fall_rain_end_day = "03-31",
                                        max_rain = 1500, # mm, right?
                                        show_cum_rainfall_plot = F,
                                        print_NAs = F,
                                        precip_thresholds = c(25, 50, #mm, ~ 1:6 inches
                                                              100, 150) ){
  
  # make copy of fall flow tab
  fft = dsb_tab
  # Get Precip data
  stn_title = "Mean Interp. Cal and FJ Precip"
  precip_file_path = file.path(local_data_dir,"obs_and_interp_precip_1935_2021.csv")
  if(file.exists(precip_file_path)){ppt = read.csv(precip_file_path)}
  if(!file.exists(precip_file_path)){
    ppt = get_interp_precip_record()
    write.csv(ppt, file = precip_file_path, row.names = F, quote = F)
  }
  
  # initialize columns in fall flow tab to receive the rainfall exceedance timing
  fft[,paste0("wy_day_exceed_",precip_thresholds,"mm")] = NA
  
  if(show_cum_rainfall_plot==T){ #initialize plot
    n_days_in_fall_leap = as.numeric(as.Date(paste0("2004-",fall_rain_end_day)) -
                                       as.Date(paste0("2003-",fall_rain_start_day))) + 1
    n_days_in_fall = as.numeric(as.Date(paste0("2002-",fall_rain_end_day)) -
                                  as.Date(paste0("2001-",fall_rain_start_day))) +1
    stn_yr_range = range(year(ppt$Date))
    plot(x=1:n_days_in_fall_leap, 
         y=seq(from=0,to=max_rain, length.out = n_days_in_fall_leap),
         xlab = "Days since Aug. 31", ylab = "Cumulative Precip. (mm)",
         pch=NA, col = NA, 
         main = stn_title,
         sub = paste(stn_yr_range, collapse = "-"))
  }
  for(i in 1:nrow(dsb_tab)){
    year_i=dsb_tab$year[i]
    fall_date1 = as.Date(paste0(year_i,"-",fall_rain_start_day)) 
    #^ choose Sept. 1 for salmon consistency. consider Oct 1 at some point
    fall_date2 = as.Date(paste0(year_i+1,"-",fall_rain_end_day))
    fall_rainfall_wy = ppt[ppt$Date >= fall_date1 &
                             ppt$Date <= fall_date2,]
    if(print_NAs){print(paste("Num NAs,",year_i, ":",sum(is.na(fall_rainfall_wy$PRCP))))}
    fall_cumppt = cumsum(fall_rainfall_wy$interp_cal_fj_mean)
    # calculate exceedances
    for(k in precip_thresholds){
      if(!is.infinite(min(which(fall_cumppt > k)))){ # if there's an exceedance date
        fft[i,paste0("wy_day_exceed_",k,"mm")] = min(which(fall_cumppt > k))
      }
    }
    if(show_cum_rainfall_plot==T){ 
      if(nrow(fall_rainfall_wy)>=n_days_in_fall){ # add line to plot if has a full rainy season
        if(nrow(fall_rainfall_wy)==n_days_in_fall){
          lines(x=1:n_days_in_fall, y = fall_cumppt, col = rgb(0.1,0.1,.8,.3))
        } else { # if in a leap year, make a new x-vector with 1 extra day
          lines(x=1:n_days_in_fall_leap, y = fall_cumppt, col = rgb(0.1,0.1,.8,.3))
        }
      }
    }
  }
  return(fft)
  
}


add_fj_recon_dates_to_fft=function(dsb_tab,
                                   fall_flow_start_day="09-01",
                                   fall_flow_end_day="03-31",
                                   flow_thresholds = c(10,20,40,50,75,100,120)){
  #initialize columns for exceedance dates
  dsb_tab[,paste0("wy_day_exceed_",flow_thresholds,"_cfs")]=NA
  for(i in 1:nrow(dsb_tab)){
    #Subset fall flows for this WY
    yr=dsb_tab$year[i] # Need the fall flows at the end of this water year. Sep-Mar of WY and WY + 1.
    ff_day1 = as.Date(paste0(yr,"-",fall_flow_start_day))
    ff_day_end =  as.Date(paste0(yr+1,"-",fall_flow_end_day))
    fall_flows = fj_flow[fj_flow$Date >= ff_day1 & fj_flow$Date <= ff_day_end,]
    # assign days in WY
    fall_flows$day_in_wy = as.numeric(fall_flows$Date - ff_day1)
    # Find exceedances
    for(k in flow_thresholds){
      dsb_tab[i,paste0("wy_day_exceed_",k,"_cfs")] = 
        min(fall_flows$day_in_wy[fall_flows$Flow >= k])
    }
  }
  return(dsb_tab)
}

one_pred_model_diagnostics = function(response = "min_fall_flow", tab_for_glm,
                                      preds = c("SWJ_max_wc_mm", "USC00043182_oct_apr_mm", "SWJ_jday_of_max", "springWL_415635N1228315W001", "et0_oct_apr", "mar_flow")){
  # initialize diagnostic table
  diag_tab = data.frame(pred1=preds, n=NA, 
                        logLike=NA,AIC=NA,loocv=NA, Rsquare=NA)
  
  for(i in 1:nrow(diag_tab)){
    pred1=as.character(diag_tab$pred1[i])
    tab_for_glm_i = tab_for_glm[!is.na(tab_for_glm[,response] * 
                                         tab_for_glm[,pred1]),]
    f1_i = paste(response," ~ ", pred1)
    glm_i = glm(f1_i, data = tab_for_glm_i)
    loocv_i = cv.glm(tab_for_glm_i, glm_i)
    lm_i = lm(f1_i, data = tab_for_glm_i) # for calculating R squared
    #attach lm results 
    diag_tab_row=diag_tab$pred1==pred1 
    diag_tab$logLike[diag_tab_row] = logLik(glm_i)
    diag_tab$AIC[diag_tab_row] = AIC(glm_i)
    diag_tab$loocv[diag_tab_row] = loocv_i$delta[1]
    diag_tab$Rsquare[diag_tab_row] = summary(lm_i)$r.squared
    diag_tab$n[diag_tab_row] = nrow(tab_for_glm_i)
    
  }
  return(diag_tab)
}

two_pred_model_diagnostics = function(response = "min_fall_flow", tab_for_glm,
                                      preds = c("SWJ_max_wc_mm", "USC00043182_oct_apr_mm", "SWJ_jday_of_max", "springWL_415635N1228315W001", "et0_oct_apr", "mar_flow")){
  
  pred_combo = as.data.frame(t(combn(preds, 2)))
  colnames(pred_combo)=c("pred1","pred2")
  # initialize diagnostic table
  diag_tab = data.frame(pred1=pred_combo$pred1, pred2=pred_combo$pred2, n=NA,
                        logLike=NA,AIC=NA,loocv=NA,Rsquare=NA)
  
  for(i in 1:nrow(pred_combo)){
    pred1=as.character(diag_tab$pred1[i])
    pred2=as.character(diag_tab$pred2[i])
    tab_for_glm_i = tab_for_glm[!is.na(tab_for_glm[,response] * 
                                         tab_for_glm[,pred1] *
                                         tab_for_glm[,pred2]),]
    f1_i = paste(response," ~ ", paste(pred1,pred2,sep=" + "))
    glm_i = glm(f1_i, data = tab_for_glm_i)
    loocv_i = cv.glm(tab_for_glm_i, glm_i)
    lm_i = lm(f1_i, data = tab_for_glm_i) # for calculating R squared
    #attach lm results 
    diag_tab_row=diag_tab$pred1==pred1 & diag_tab$pred2==pred2
    diag_tab$logLike[diag_tab_row] = logLik(glm_i)
    diag_tab$AIC[diag_tab_row] = AIC(glm_i)
    diag_tab$loocv[diag_tab_row] = loocv_i$delta[1]
    diag_tab$Rsquare[diag_tab_row] = summary(lm_i)$r.squared
    diag_tab$n[diag_tab_row] = nrow(tab_for_glm_i)
  }
  return(diag_tab)
}

three_pred_model_diagnostics = function(response = "min_fall_flow", tab_for_glm,
                                        preds = c("SWJ_max_wc_mm","USC00043182_oct_apr_mm",
                                                  "SWJ_jday_of_max","springWL_415635N1228315W001",
                                                  "et0_oct_apr","mar_flow")){
  
  pred_combo = as.data.frame(t(combn(preds, 3)))
  colnames(pred_combo)=c("pred1","pred2","pred3")
  # initialize diagnostic table
  diag_tab = pred_combo
  diag_tab$n = NA;  diag_tab$logLike = NA; 
  diag_tab$AIC = NA; diag_tab$loocv=NA
  
  for(i in 1:nrow(pred_combo)){
    pred1=as.character(diag_tab$pred1[i])
    pred2=as.character(diag_tab$pred2[i])
    pred3=as.character(diag_tab$pred3[i])
    tab_for_glm_i = tab_for_glm[!is.na(tab_for_glm[,response] * 
                                         tab_for_glm[,pred1] *
                                         tab_for_glm[,pred2] *
                                         tab_for_glm[,pred3]),]
    f1_i = paste(response," ~ ", paste(pred1,pred2,pred3,sep=" + "))
    glm_i = glm(f1_i, data = tab_for_glm_i)
    loocv_i = cv.glm(tab_for_glm_i, glm_i)
    lm_i = lm(f1_i, data = tab_for_glm_i) # for calculating R squared
    #attach lm results 
    diag_tab_row=diag_tab$pred1==pred1 & diag_tab$pred2==pred2 & diag_tab$pred3==pred3
    diag_tab$logLike[diag_tab_row] = logLik(glm_i)
    diag_tab$AIC[diag_tab_row] = AIC(glm_i)
    diag_tab$loocv[diag_tab_row] = loocv_i$delta[1]
    diag_tab$Rsquare[diag_tab_row] = summary(lm_i)$r.squared
    diag_tab$n[diag_tab_row] = nrow(tab_for_glm_i)
  }
  
  return(diag_tab)
}



make_table_of_selected_model_coefficients = function(){}

# Figure functions --------------------------------------------------------


watershed_behavior_fig = function(wy=2016){
  # wy = 2019
  d1 = as.Date(paste0(wy-1, "-09-01"))
  d2 = as.Date(paste0(wy, "-09-01"))
  
  fj_plot = fj_flow[fj_flow$Date >= d1 & fj_flow$Date <=d2,]
  
  #make space for 2nd axis
  par(mar=c(5,4,3,5))
  # Plot FJ flow in example water year
  plot(fj_plot$Date, fj_plot$Flow * cfs_to_m3day/10^6, type = "l", 
       lwd = 2, col="dodgerblue",log="y", yaxt="n", xaxt="n", 
       ylim=c(min(fj_plot$Flow * cfs_to_m3day/10^6), 100),
       main = "Example Scott River annual hydrograph",
       xlab = paste0("Date in modified water year ",wy, " (starting Sep. 1)"), 
       ylab = "Fort Jones gauge flow (Mm3 per day)")
  abline(v=as.Date(paste0(wy-1,"-10-01")), lty = 2, lwd =2)
  # Custom date and flow axis labels
  axis_dates = seq.Date(from=d1,to=d2,by="2 months")
  axis_date_labs = format(axis_dates,"%b %d"); axis_date_labs[c(2,4,6)]=NA
  abline(h=1 * 10^(-2:3), v= axis_dates, lty=3, col = "gray")#grid
  axis(side = 1, at = axis_dates, labels = axis_date_labs, las = 1)
  flow_labels = c(".001","0.1", "1", "10", "100","1,000")
  axis(side = 2, at = 1 * 10^(-2:3), labels = flow_labels, las = 1)
  axis(side = 2, at = 2:9 * 10^(sort(rep(-2:3,9))), tck = -.01, labels=NA)
  # Annotate date ranges with arrows - WY 2019
  arrH=50; textH=80
  # b1 = d1; b2=as.Date("2018-10-01"); b3=as.Date("2018-11-22")# for WY 2016
  # b4 = as.Date("2019-06-01"); b5=as.Date("2019-07-01");
  # b6=as.Date("2019-08-05")
  b1 = d1; b2=as.Date("2015-11-01"); b3=as.Date("2015-11-29")
  b4 = as.Date("2016-06-15"); b5=as.Date("2016-08-01");
  b6=as.Date("2016-09-01")
  # A, Dry season baseflow
  arrows(x0=d1, x1=b2, 
         y0=arrH, y1=arrH, code = 3, angle=90, length=0.1)
  text(x=d1, y=textH, pos=4, label= "A")
  # B, Moderate fall flow increase
  arrows(x0=b2, x1=b3, 
         y0=arrH, y1=arrH, code = 3, angle=90, length=0.1)
  text(x=mean(c(b2,b3)), y=textH, label= "B")
  # C and D, intermittent winter stormflow and winter baseflow
  arrows(x0=b3, x1=b4, 
         y0=arrH, y1=arrH, code = 3, angle=90, length=0.1)
  text(x=mean(c(b3,b4)), y=textH, label= "C with D events")
  # C, winter baseflow (spring recession)
  arrows(x0=b4, x1=b5, 
         y0=arrH, y1=arrH, code = 3, angle=90, length=0.1)
  text(x=mean(c(b4,b5)), y=textH, label= "C")
  # A, dry season baseflow (spring recession)
  arrows(x0=b5, x1=b6, 
         y0=arrH, y1=arrH, code = 3, angle=90, length=0.1)
  text(x=mean(c(b5,b6)), y=textH, label= "A")
  
  # prep cumulative precip. 
  interp_precip = read.csv(file.path(local_data_dir, "obs_and_interp_precip_1935_2021.csv"))
  interp_precip = interp_precip[,c("Date","interp_cal_fj_mean")]
  interp_precip$Date=as.Date(interp_precip$Date)
  ppt_wy = interp_precip[interp_precip$Date >= d1 & interp_precip$Date <= d2,]
  ppt_wy$cumppt = cumsum(ppt_wy$interp_cal_fj_mean)
  
  # make 2nd y-axis
  precip_col_ch3 = "darkorchid"
  par(new=T)
  plot(x = ppt_wy$Date, ppt_wy$cumppt, 
       axes=F, bty="n",xlab="",ylab="", type = "l", 
       col = precip_col_ch3, lwd=2, lty =1, ylim = c(0,700)) #ylim = c(0,1000))
  axis(side = 4, at = pretty(range(ppt_wy$cumppt)) )
  mtext(text = "Cumulative precip (mm; interp. Cal-FJ mean)",
        side = 4, line = 2.5)
  
  legend(x="bottom", lwd = 2, lty=c(1,1,2), cex = .85,
         col = c("dodgerblue",precip_col,"black"),
         legend=c("Fort Jones flow","Cum. Precip","Oct. 1 (trad. WY day 1)"))
  
}


watershed_fig_ch3 = function(){
  color_basin = "brown"
  #Process cimis location as spatial object
  cimis_loc = data.frame(latitude = 41.57809840006446, 
                         longitude = -122.83775052587094,
                         id_num = 225)
  cimis_loc = st_as_sf(cimis_loc, coords = c("longitude","latitude"),
                       crs=crs("+init=epsg:4326"))
  cimis_loc = st_transform(cimis_loc,  crs("+init=epsg:3310"))
  
  #subset other spatial highlights  
  fj_sp = sw_scott_sp[sw_scott_sp$site_no == fj_num,][1,]
  fjrs_sp = noaa_station_sp[noaa_station_sp$station.id =="USC00043182",]
  SWJ_sp = snow_stn_sp[snow_stn_sp$station_id=="SWJ",]
  # make table of data obs. types
  obs_locs = data.frame(type_descrip = c("FJ Gauge", "CIMIS Stn.", "Snow Stn.", "Weather Stn."),
                        type_pch = c(22, 23, 24, 25),
                        type_col = colorblind_pal()(5)[2:5])
  
  #plot
  plot(watershed$geometry)
  #read raster in from file, since the saved-in-RData raster objects are giving me a hard time right now
  plot(hill_wsh, col = hillshade_palette_faded(10), 
       legend = F, add=T)
  plot(mapped_streams$geometry, add=T, lwd = 2, col = color_tribs)
  plot(riv$geometry, add=T, lwd = 3, col = color_river)
  plot(watershed$geometry, add=T, lwd = 2, border = color_watershed)
  plot(basin$geometry, add=T, lwd = 1.5, border = color_basin)
  plot(cimis_loc$geometry, add=T, pch = 23, bg = obs_locs$type_col[2], cex = 1.5)
  plot(snow_stn_sp$geometry, add=T, pch= 24, bg = obs_locs$type_col[3])
  plot(noaa_station_sp$geometry, add=T, pch=25, bg = obs_locs$type_col[4])
  # Plot and label highlighted objects
  plot(fj_sp$geometry, add=T, pch = 22, bg = obs_locs$type_col[1], cex = 2)
  plot(SWJ_sp$geometry, add=T, pch = 24, bg = obs_locs$type_col[3], cex = 1.7)
  plot(fjrs_sp$geometry, add=T, pch=25, bg = obs_locs$type_col[4], cex = 1.7)
  # get label locations
  SWJ_xy = st_coordinates(SWJ_sp); fj_xy = st_coordinates(fj_sp); fjrs_xy= st_coordinates(fjrs_sp)
  text( x = SWJ_xy, label = "SWJ", pos = 3)
  text(x = fj_xy, labels = "FJ Gauge", pos = 3)
  text(x = fjrs_xy, labels = "FJRS", pos = 4)
  # add legend
  legend(x = "bottomright", legend = obs_locs$type_descrip, bg = "white",
         pch = obs_locs$type_pch, pt.bg = obs_locs$type_col, pt.cex = 1.5)
  legend(x = "bottomleft", lwd = c(2,1.5,2,3), 
         legend = c("Watershed HUC8", "Groundwater Basin", "Tributary Stream", "Scott River"), 
         col = c(color_watershed, color_basin, color_tribs, color_river), bg="white")
  north(xy="topleft",type=1) # add north arrow
  sbar_x = st_bbox(watershed)["xmin"] + -.25 * diff(c(st_bbox(watershed)["xmin"],st_bbox(watershed)["xmax"]))
  sbar_y = st_bbox(watershed)["ymin"] + .25 * diff(c(st_bbox(watershed)["ymin"],st_bbox(watershed)["ymax"]))
  sbar(d=20*10^3, xy=c(sbar_x, sbar_y), label = c("0","10","20"), 
       type = "bar", divs = 4,
       below = "km", adj = c(1, -1.4))
}


gw_vs_fall_flows_data_exp = function(fall_flow_window_tab, spring_months,
                                     save_well_by_well_pdf = F,
                                     show_well_by_well_plots = F,
                                     show_corr_map_plots = F,
                                     min_obs_for_cor = 3){
  
  ff_color = fall_flow_window_tab
  wl_obs$year = year(wl_obs$date)
  ff_max = max(dsb_tab[,2:(1+nrow(ff_color))]) / 10^6 # for well-by-well plots
  
  #Initialize table of correlation coefficients
  cor_tab = data.frame(well_code = wells$well_code,
                       agg1_cor=NA,agg2_cor=NA,agg3_cor=NA,
                       agg4_cor=NA,agg5_cor=NA)
  colnames(cor_tab) = c("well_code", paste0(ff_color$agg_window,"_cor"))
  m_tab = data.frame(well_code = wells$well_code,
                     agg1_cor=NA,agg2_cor=NA,agg3_cor=NA,
                     agg4_cor=NA,agg5_cor=NA)
  colnames(m_tab) = c("well_code", paste0(ff_color$agg_window,"_slope"))
  n_tab = data.frame(well_code = wells$well_code,
                     agg1_cor=NA,agg2_cor=NA,agg3_cor=NA,
                     agg4_cor=NA,agg5_cor=NA)
  colnames(n_tab) = c("well_code", paste0(ff_color$agg_window,"_n_obs"))
  
  if(save_well_by_well_pdf==T){ # Initialize pdf if saving pdf
    file_name = paste0(month.abb[min(spring_months)],"-",month.abb[max(spring_months)], 
                       "_gw_vs_fall_flows.pdf")
    pdf(file=file_name, height = 11/2, width = 8.5)}
  
  # num_corr_wells=0
  for(i in 1:nrow(wells)){
    well_id = wells$well_code[i]
    # check if there's any obs. for this well in spring window
    selector = wl_obs$well_code==well_id & month(wl_obs$date) %in% spring_months
    if(sum(selector)>0){ 
      # if(sum(selector)>min_obs_for_cor){
      #   num_corr_wells = num_corr_wells+1
      #   print(paste(well_id, "has ", sum(selector), "spring obs"))}
      # Subset wl_obs to just spring
      spring_elev = wl_obs[selector,c("date","wse_ft","year")]
      # add fall flow aggregates by year
      spring_elev_fall_flow = merge(spring_elev, dsb_tab, by.x = "year", by.y="year")
      seff = spring_elev_fall_flow
      
      if(show_well_by_well_plots==T){ # Initialize plot if we are plotting
        plot(seff$wse_ft, y = rep(NA,nrow(seff)), 
             main = well_id, 
             log = "y", yaxt = "n",
             pch = 21, bg = ff_color$color[1], 
             cex = 1.5, ylim = c(0.1, ff_max),
             ylab = "Fall Flow Vol (million m3)", 
             xlab = paste0(month.abb[min(spring_months)], "-",
                           month.abb[max(spring_months)]," GW Elev, ft amsl, ",
                           paste(range(seff$year),collapse="-")))
        # add custom y-axis and gridlines
        axis(side = 2, at = 1 * 10^(-1:3), labels = flow_labels, las = 1)
        axis(side = 2, at = 2:9 * 10^(sort(rep(-1:3,8))), tck = -.01, labels=NA)
        abline(h=1 * 10^(-1:3), lty = 3, col = "gray", v=pretty(range(seff$wse_ft)))
      }
      for(j in 1:nrow(ff_color)){ #Iterate through aggr. windows
        
        if(show_well_by_well_plots==T){ # Add points to plot if we are plotting
          points(seff$wse_ft, seff[,paste0(ff_color$agg_window[j],"_flow")] / 10^6, pch = 21, 
                 bg = ff_color$color[j], cex = 1.5)
        }
        
        if(sum(!is.na(seff$wse_ft))>min_obs_for_cor){ # If there's enough obs. for corr.
          cor_tab[i,paste0(ff_color$agg_window[j],"_cor")] = 
            cor(x=seff$wse_ft, y=seff[,paste0(ff_color$agg_window[j],"_flow")]/10^6)

          lm_i = lm(seff[,paste0(ff_color$agg_window[j],"_flow")]/10^6 ~ seff$wse_ft)
          m_tab[i,paste0(ff_color$agg_window[j],"_slope")] =
            lm_i$coefficients[2]
          
          n_tab[i, paste0(ff_color$agg_window[j],"_n_obs")] = nrow(seff)
        }
      }
      if(show_well_by_well_plots==T){
        legend(x="topleft",legend=ff_color$agg_window, pt.bg=ff_color$color, 
               pch=21, pt.cex=1.5)
      }
    }
  }
  
  if(save_well_by_well_pdf==T){dev.off()}
  
  # attach correlation coeffs to the spatial dataset
  wells_sp_cor = terra::merge(wells_sp, cor_tab, by.x = "well_code", by.y = "well_code")
  # Set up for plotting corr. coeff
  cor_breaks = seq(from=-1, to = 1, by = .25)
  n_classes = length(cor_breaks)-1
  my_palette_name = "Blue-Red"# "Oslo"
  my_palette = rev(diverging_hcl(n_classes, 
                                 palette = my_palette_name))
  
  if(show_corr_map_plots==T){
    # for(i in 1:nrow(ff_color)){ # actually, just plot for the one september one.
    sep_index = which(ff_color$agg_window=="sep")
    for(i in sep_index){
      colname_window = paste0(ff_color$agg_window[i],"_cor")
      
      classification_vector = cut(st_drop_geometry(wells_sp_cor)[,colname_window],
                                  include.lowest=T,
                                  breaks = cor_breaks)
      color_vector = my_palette[classification_vector]
      
      # Initialize plot with basin and titles
      plot(basin$geometry,
           main = paste( "Correlation coefficients between April groundwater \n elevations and subsequent September flow volume")
      )
      # Plot the color-coded wells
      plot(wells_sp_cor$geometry, pch=19, col=color_vector, #pch=21, bg = color_vector, 
           add=T, cex = 1.5)
      legend_text = paste(cor_breaks[1:(n_classes)], 
                          cor_breaks[2:(n_classes+1)], sep = " to ")
      legend(x="bottomleft", title = "Corr. Coeff. Value",#pt.bg = my_palette, pch=21,
             pt.cex=1.5, col = my_palette, pch=19,
             legend = legend_text)
    }
  }
  
  return(list(cor_tab,m_tab,n_tab))
  
}

  

gw_vs_p_spill_data_exp = function(well_ids = "all", 
                                  save_well_by_well_pdf = F,
                                  plot_composite_wls_scatter = F,
                                  add_lm_line = F){
  
  gw_month_tab = data.frame(month = 1:12, #color = rainbow(n=10)[c(6:10,1:5)])
                            color = terrain.colors(n=12)[c(1:12)])
  
  
  if(save_well_by_well_pdf==T){ # Initialize pdf if saving pdf
    file_name = paste0("gw_vs_p_spill.pdf")
    pdf(file=file_name, height = 11/2, width = 8.5)}
  
  if(well_ids == "all"){  # subset wells for plotting
    wells_plot = wells
  } else {
    wells_plot = wells[wells$well_code %in% well_ids,] }
  
  
  for(i in 1:nrow(wells_plot)){
    well_id = wells_plot$well_code[i]
    # check if there's any obs. for this well in spring window
    wl_obs_i = wl_obs[wl_obs$well_code==well_id,]
    if(nrow(wl_obs_i)>0){
      # wl_obs_ij = wl_obs_i[month(wl_obs_i$date) == j,]
      p_spill_i = dsb_tab$cum_ppt_on_first_day_qspill[match(year(wl_obs_i$date), 
                                                                  dsb_tab$year)] 
      gw_month_color = gw_month_tab$color[match(month(wl_obs_i$date),
                                                gw_month_tab$month)]
      plot(x = wl_obs_i$wse_ft, y = p_spill_i, pch=21,
           bg = gw_month_color, main = well_id, cex = 2,
           xlab = "GW Elev. (ft amsl)", ylab = "P spill (cum. precip, mm)",
           sub = paste(range(year(wl_obs_i$date),na.rm=T), collapse = "-"))
      grid()
      legend(x="topleft",pch=21,pt.bg = gw_month_tab$color, cex =.8, title = "GW meas. in:",
             legend = month.abb[gw_month_tab$month], ncol=2)
      
      if(add_lm_line == T){
        lm_well = lm(formula = p_spill_i ~ wl_obs_i$wse_ft)
        abline(lm_well)
        text1 = paste0("Slope: ", round(lm_well$coefficients[2], digits = 2))
        text2 = paste0("R square: ", round(summary(lm_well)$r.square, digits = 2))
        legend(x= "bottomleft", col = NA,legend = c(text1, text2),
               bg=NA, bty="n")
        
      }
      
    }
  }
  
  # spring flows?
  # plot(x=dsb_tab$apr_flow, y=dsb_tab$cum_ppt_on_first_day_qspill)
  
  if(save_well_by_well_pdf==T){ dev.off()}
  
  if(plot_composite_wls_scatter == T){
    # Ugh. try a composit WL score?
    # calc WL mean
    mean_wls = aggregate(wl_obs$wse_ft, by = list(wl_obs$well_code), FUN = mean, na.rm=T)
    colnames(mean_wls) = c("well_code", "mean_wse")
    sd_wls =  aggregate(wl_obs$wse_ft, by = list(wl_obs$well_code), FUN = sd, na.rm=T)
    colnames(sd_wls) = c("well_code", "sd_wse")
    
    wells1 = merge(wells, mean_wls, by.x = "well_code",by.y = "well_code")
    wells1 = merge(wells1, sd_wls, by.x="well_code",by.y="well_code")

    par(mfrow = c(3,2))
    for(i in 1:12){
      # dsb_tab[,paste0("comp_wl_",month.abb[i])]=NA
      wl_obs_i = wl_obs[month(wl_obs$date)==i,]
      wl_obs_i$year = year(wl_obs_i$date)
      wl_std = (wl_obs_i$wse - 
                  wells1$mean_wse[match(wl_obs_i$well_code, wells1$well_code)]) /
        wells1$sd_wse[match(wl_obs_i$well_code, wells1$well_code)]
      # summary(wl_std)
      comb_by_yr = aggregate(wl_std, by = list(wl_obs_i$year), FUN = median)
      colnames(comb_by_yr) = c("year",paste0("comb_wl_", month.abb[i]))
      fft_plot = merge(dsb_tab, comb_by_yr, by.x = "year", by.y = "year")
      
      plot(x=fft_plot[,paste0("comb_wl_", month.abb[i])],
           y = fft_plot$cum_ppt_on_first_day_qspill, 
           pch = 21, bg = gw_month_tab$color[i], cex = 2,
           main = month.abb[i])
      grid()
    }
  }
  
}



snow_vs_fall_flows = function(xtype = "Water Content"){
  
  if(xtype == "Water Content"){
    # plot max water content vs Sep flows
    par(mfrow = c(4,2))
    for(i in 1:length(snow_stns)){
      stn = snow_stns[i]
      max_WC = dsb_tab[,paste0(stn,"_max_wc_mm")]
      flow_Mm3 = dsb_tab$sep_flow / 10^6
      plot(x=max_WC,
           y = dsb_tab$sep_flow / 10^6,
           xlim = c(0,85), ylim = c(0,20),
           xlab = "Max Water Content (in.)", ylab = "Sep. Flow Vol (Mm3)",
           main = stn, pch = 19, col = rainbow(8)[i])
      grid()
      lm_stn = lm(flow_Mm3 ~ max_WC)
      abline(lm_stn, lty = 2) # add line of best fit
      
      text1 = paste0("Slope: ", round(lm_stn$coefficients[2], digits = 2))
      text2 = paste0("R square: ", round(summary(lm_stn)$r.square, digits = 2))
      legend(x= "topleft", col = NA,legend = c(text1, text2),
             bg=NA, bty="n")
      
    }
  }
  
  if(xtype == "Julian Day"){
    # plot max water content TIMING vs Sep flows
    par(mfrow = c(4,2))
    for(i in 1:length(snow_stns)){
      stn = snow_stns[i]
      max_jdays = dsb_tab[,paste0(stn,"_jday_of_max")]
      flow_Mm3 = dsb_tab$sep_flow / 10^6
      plot(x= max_jdays,
           y = flow_Mm3, xlim = c(10,130), ylim = c(0,20),
           xlab = "Julian Day of Max Water Content", ylab = "Sep. Flow Vol (Mm3)",
           main = stn, pch = 19, col = rainbow(8)[i])
      grid()
      lm_stn = lm(flow_Mm3 ~ max_jdays)
      abline(lm_stn, lty = 2) # add line of best fit
      
      text1 = paste0("Slope: ", round(lm_stn$coefficients[2], digits = 2))
      text2 = paste0("R square: ", round(summary(lm_stn)$r.square, digits = 2))
      legend(x= "topleft", col = NA,legend = c(text1, text2),
             bg=NA, bty="n")
      
    }
  }
  
}

Q_spill_threshold_tester = function(results, Q_spill = 120, q_thresh,fj3d){
  # prep tables
  EoDS_years = 1943:2020
  # prep fj3d table for plotting dq dp
  fj3d = add_dQ_dP_cols(fj3d = fj3d, lag_days = 1,  Q = fj3d$Flow_m3day, P = fj3d$cum_ppt)
  # pick thresholds for plot
  q_thresh = round(seq(from = 10, by = 5, to = 500) * cfs_to_m3day)
  EoDS_years = 1943:2020
  # calculate dq and dp values for dry and wet season for each threshold
  results = dQ_dP_difference_finder(q_thresh=q_thresh, EoDS_years)
  
  #summarize
  results$dQ_dP_pre = results$dQ_cum_pre/results$dP_cum_pre
  results$dQ_dP_post = results$dQ_cum_post/results$dP_cum_post
  results$delta_dQdP = results$dQ_dP_post - results$dQ_dP_pre
  
  dq_pre_mean = aggregate(x = results$dQ_cum_pre,
                          by = list(results$Q_threshold_m3day), FUN = mean,
                          na.rm=T,finite=T)$x
  dq_post_mean = aggregate(x = results$dQ_cum_post,
                           by = list(results$Q_threshold_m3day), FUN = mean,
                           na.rm=T,finite=T)$x
  dp_pre_mean = aggregate(x = results$dP_cum_pre,
                          by = list(results$Q_threshold_m3day), FUN = mean,
                          na.rm=T,finite=T)$x
  dp_post_mean = aggregate(x = results$dP_cum_post,
                           by = list(results$Q_threshold_m3day), FUN = mean,
                           na.rm=T,finite=T)$x
  
  delta_dq_dp = dq_post_mean/dp_post_mean - dq_pre_mean/dp_pre_mean
  
  # Plot wet, dry and difference of dQ/dP
  par(mar = c(5,8,4,1))
  plot(q_thresh/cfs_to_m3day, dq_pre_mean/dp_pre_mean/cfs_to_m3day,
       main = "Spill thresholds vs 78-year average of \n dry season rainfall-runoff responses (dQ/dP)",
       ylim = c(-20,300), pch = 19, type = "o", col = "darkgoldenrod3", cex=.7,
       ylab = "Ratio of 30-day cumulative flow difference \n (dQ, m3) to 30-day precip. (dP, mm)",
       xlab = "Flow threshold (cfs) defining watershed spill condition \n (dividing dry and wet season)")
  lines(q_thresh/cfs_to_m3day, dq_post_mean/dp_post_mean/cfs_to_m3day, 
        pch = 15, type = "o", col = "dodgerblue", cex = .7)
  lines(q_thresh/cfs_to_m3day, delta_dq_dp/cfs_to_m3day,
        pch=17, type = "o", col = "firebrick", cex=.7)
  grid()
  abline(v=Q_spill, col = "brown", lty = 3, lwd = 2)
  abline(v=Q_spill, lwd=30, col = rgb(1,0,0,.2))
  # abline(v=80)
  legend(x="bottomright", 
         legend = c("Avg. dry season dQ/dP", "Avg. wet season dQ/dP", "dQ/dP diff. (wet minus dry)","Approx. dQ/dP diff. plateau"),
         lty=c(1,1,1,3), lwd=c(1,1,1,2), col = c("darkgoldenrod3", "dodgerblue", "firebrick","brown"),
         pch=c(19,15,17, NA), cex = .7)
}


dQdP_vs_hydrograph = function(fj3d,
                              date_1 = as.Date("2015-09-01"), 
                              date_2 = as.Date("2016-02-01"),
                              as_png = F, 
                              max_lag_days = 1,
                              Q_spill_threshold = 120){
  
  if(as_png == TRUE){
    png(filename = file.path(scratch_dir,paste("dqdp vs hydrograph", date_1, "_",date_2,".png")), width = 8.5, height = 11, unit = "in", res = 300)
  }
  
  # subset the flow and precip table by date for hydrograph
  fj3d_dates = fj3d[fj3d$Date>=date_1 & fj3d$Date <=date_2,]
  
  
  
  # Panel 2: hydrograph
  # find first day of spilling
  first_spill_day = min(fj3d_dates$Date[fj3d_dates$Flow >= Q_spill_threshold], na.rm=T)
  
  # plot hydrograph
  plot(fj3d_dates$Date, fj3d_dates$Flow,# * cfs_to_m3day, #fj3d_dates$cum_flow_m3, 
       log = "y", ylim=range(fj3d$Flow,na.rm=T),# * cfs_to_m3day, na.rm=T),
       main = paste0("Fort Jones Gauge Flow and Cum. Precip., ", year(date_1), "-", year(date_2)), 
       type ="o",pch=18, lwd = 2, col = "blue",
       ylab = "Fort Jones daily flow (cfs)", xlab = "")
  grid()
  abline(v=first_spill_day, lty=2, lwd=2,col = "red")
  # add spill line (reference to flow axis on left)
  arrows(x0=fj3d_dates$Date[1], x1=first_spill_day, 
         y0=Q_spill_threshold, col = "dodgerblue", lty = 2, code = 0)
  
  P_spill=fj3d_dates$cum_ppt[fj3d_dates$Date==first_spill_day]
  
  legend(x="topleft", lwd = c(1,1,2,2,2), lty = c(1,1,3,2,5), pch = c(18,3,NA,NA,NA),
         col = c("blue","violet", "dodgerblue","red", "mediumorchid"), xjust=0.5,
         legend = c("Fort Jones flow","Cum. Precip.", 
                    paste0("Q spill (",Q_spill_threshold," cfs)"),
                    paste0("First spill day, WY ",year(date_2)),
           paste0("P spill, WY ",year(date_2)," (",round(P_spill), " mm)")),
         cex=.8)
  
  #Add arrows indicating 30-day windows for calculating dQdP
  arrows(x0=first_spill_day, x1=first_spill_day-29, y0=1E7, code = 3)
  arrows(x0=first_spill_day, x1=first_spill_day+29, y0=1E7, code=3)
  text(x=first_spill_day, y = 4E7, labels="dQ/dP calculation windows \n to determine Q spill")
  
  # Plot cumulative precip on right axis 
  par(new=TRUE)
  plot(fj3d_dates$Date, fj3d_dates$cum_ppt, 
       axes=F, ylab="",xlab="", ylim = range(fj3d$cum_ppt, na.rm=T),
       type = "o", pch=3, col = "violet", lwd = 2)
  axis(side=4, at = pretty(range(fj3d_dates$cum_ppt)))
  mtext("Cum. Precip. since Sept. 01 (mm)", side = 4, line = 3)
  # to do: add pSpill line (reference to ppt axis on right)
  arrows(x1=fj3d_dates$Date[nrow(fj3d_dates)], x0=first_spill_day,
         y0=P_spill, col = "mediumorchid", lty = 5, lwd=2, code = 0)
  
  if(as_png==TRUE){dev.off()}
  
}



time_rain_flow_fig = function(wys = 1944:2021, plot_panel = NA,
                              cum_precip_arrows, add_percentile_lines=T,
                              flow_threshold, P_max){
  
  # Panel 1
  if(is.na(plot_panel) | plot_panel == 1){
    # plot WY days vs flow (1 line per year)
    for(i in 1:length(wys)){
      wy = wys[i]
      if(i==1){
        plot(x = range(fj3d$days_after_aug31), yaxt="n",
             y = range(fj3d$Flow, na.rm=T), pch = 18, col = NA, log = "y",
             xlab = "Day of modified WY, starting Sept. 1", ylab = "FJ Flow (cfs)")
        grid()
      } 
      fj3d_indices = fj3d$wy_3d==wy
      lines(x = fj3d$days_after_aug31[fj3d_indices], 
            y = fj3d$Flow[fj3d_indices], 
            # col = era_color)
            col = rgb(.5,.1,.5,.3))
      
    }
    #Add useful axis ticks
    axis(side = 2, at = 10^(-2:4), labels = 10^(-2:4))
    axis(side = 2, at = rep(1:9,4)*sort(rep(10^(-2:4),9)), labels = NA, tck = -0.01)
    abline(h=flow_threshold, lty=2, lwd=2)
    #Annotate with flow threshold
    legend(x="bottomright",lwd=2,lty=2,
           legend = paste0("FJ Flow \'Q spill\' Threshold (", round(flow_threshold)," cfs)"))
    # abline(h= flow_threshold, lty = 2, lwd = 2)
    if(is.na(plot_panel)){legend(x="topright",bty="n",legend="A", cex = 1.5)}# Panel label
    
  }

  
  # Panel 2
  if(is.na(plot_panel) | plot_panel == 2){
    # plot cum precip vs flow (1 line per year)
    for(i in 1:length(wys)){
      wy = wys[i]
      if(i==1){
        plot(x = range(fj3d$cum_ppt,na.rm=T), yaxt="n",
             y = range(fj3d$Flow, na.rm=T), pch = 18, col = NA, log = "y",
             xlab = "Cumulative Precip., Sep. 1 - Mar. 31, FJ Ranger Stn (mm)", ylab = "FJ Flow (cfs)")
        grid()
      } 
      fj3d_indices = fj3d$wy_3d==wy
      lines(x = fj3d$cum_ppt[fj3d_indices], 
            y = fj3d$Flow[fj3d_indices], 
            # col = alpha(era_color,.4))
            col = rgb(.1,.5,.5,.3))
    }
    #Add useful axis ticks
    axis(side = 2, at = 10^(-2:4), labels = 10^(-2:4))
    axis(side = 2, at = rep(1:9,4)*sort(rep(10^(-2:4),9)), labels = NA, tck = -0.01)
    # annotate with lag calc point
    # legend(x="bottomright",pt.lwd=3,pch=3,
    #        legend = paste0("P Max and Q spill"))
    # points(x=P_max, y=flow_threshold,pch=3, cex = 2, lwd=3)
    arrows(x0=cum_precip_arrows[1], x1=cum_precip_arrows[2],
           y0=flow_threshold, y1=flow_threshold,
           angle=90, length=.1, lty=1, code=3)
    text(x=160, y=30, labels = "Range of P spill at Q spill = 120 cfs",pos=4)# label range
    if(is.na(plot_panel)){legend(x="topright",bty="n",legend="B", cex = 1.5)}# Panel label
  }
  if(add_percentile_lines == TRUE){
    # Calculate and add percentile lines to plot
    num_bins = 50
    x_breaks = seq(from=range(fj3d$cum_ppt,na.rm=T)[1], to = range(fj3d$cum_ppt,na.rm=T)[2],
                   length.out=num_bins+1)
    y_10th = rep(NA,num_bins); y_50th = rep(NA,num_bins); y_90th = rep(NA,num_bins);
    for(i in 1:num_bins){
      binned_y_vals = fj3d$Flow[fj3d$cum_ppt>=x_breaks[i] & 
                                  fj3d$cum_ppt<x_breaks[i+1]]
      if(i==num_bins){binned_y_vals = fj3d$Flow[fj3d$cum_ppt>=x_breaks[i] & fj3d$cum_ppt<=x_breaks[i+1]]}
      y_10th[i] = quantile(x=binned_y_vals,probs=0.1, na.rm=T)
      y_50th[i] = quantile(x=binned_y_vals,probs=0.5, na.rm=T)
      y_90th[i] = quantile(x=binned_y_vals,probs=0.9, na.rm=T)
    }
    lines(x = x_breaks[1:num_bins], y = y_10th, lty = 2)
    lines(x = x_breaks[1:num_bins], y = y_90th, lty = 2)
    lines(x = x_breaks[1:num_bins], y = y_50th, lwd = 2)
    
    legend(x="topleft",legend = c("50th flow %ile","10th and 90th"),lwd=c(2,1),lty=c(1,2))
  }
  

  # Panel 3
  if(is.na(plot_panel) | plot_panel == 3){
    # plot WY days vs cumulative precip (1 line per year)
    for(i in 1:length(wys)){
      wy = wys[i]
      if(i==1){
        plot(x = range(fj3d$days_after_aug31), 
             y = range(fj3d$cum_ppt, na.rm=T), pch = 18, col = NA,
             xlab = "Day of modified WY, starting Sept. 1", 
             ylab = "Cum. Precip., FJ Ranger Stn (mm)")
        grid()
      } 
      fj3d_indices = fj3d$wy_3d==wy
      era_col = era_tab$color_minflow[era_tab$era == dsb_tab$era[dsb_tab$year==wy]]
      lines(x = fj3d$days_after_aug31[fj3d_indices], 
            y = fj3d$cum_ppt[fj3d_indices], 
            col = era_col)
            # col = rgb(.5,.5,.1,.3))
      
    }
    # #Annotate with cumulative precip threshold
    # legend(x="topleft",lwd=2,lty=2,
    #        legend = paste0("Maximum P spill value, 1942-2021 (",round(P_max)," mm)"))
    # abline(h=P_max, lty = 2, lwd = 2)
    era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
    if(add_percentile_lines == FALSE){
      legend(x="topleft",lwd=2, col = era_tab$color_minflow,
             legend = paste0("Era ",era_tab$era,": ",era_yrs_text))
    }

    if(is.na(plot_panel)){legend(x="topright",bty="n",legend="C", cex = 1.5)}# Panel label
  }

  if(add_percentile_lines == TRUE){
    #add era to fj3d
    fj3d$era = 1
    fj3d$era[fj3d$wy_3d>=1977 & fj3d$wy_3d<=2000] = 2
    fj3d$era[fj3d$wy_3d>=2001] = 3
    
    # Calculate and add percentile lines to plot
    num_bins = 20
    x_breaks = seq(from=range(fj3d$days_after_aug31,na.rm=T)[1], to = range(fj3d$days_after_aug31,na.rm=T)[2],
                   length.out=num_bins+1)
    y_e1 = rep(NA,num_bins); y_e2 = rep(NA,num_bins); y_e3 = rep(NA,num_bins);
    for(i in 1:num_bins){
      bin_picker = fj3d$days_after_aug31>=x_breaks[i] & fj3d$days_after_aug31<x_breaks[i+1]
      binned_yvals_e1 = fj3d$cum_ppt[bin_picker & fj3d$era==1]
      binned_yvals_e2 = fj3d$cum_ppt[bin_picker & fj3d$era==2]
      binned_yvals_e3 = fj3d$cum_ppt[bin_picker & fj3d$era==3]
      if(i==num_bins){binned_y_vals = fj3d$cum_ppt[fj3d$days_after_aug31>=x_breaks[i] & fj3d$days_after_aug31<=x_breaks[i+1]]}
      y_e1[i] = quantile(x=binned_yvals_e1,probs=0.5, na.rm=T)
      y_e2[i] = quantile(x=binned_yvals_e2,probs=0.5, na.rm=T)
      y_e3[i] = quantile(x=binned_yvals_e3,probs=0.5, na.rm=T)
    }
    lines(x = x_breaks[1:num_bins], y = y_e1, lty = 1, lwd = 2)#, col = era_tab$color_minflow[era_tab$era==1])
    lines(x = x_breaks[1:num_bins], y = y_e2, lty = 2, lwd = 2)#, col = era_tab$color_minflow[era_tab$era==2])
    lines(x = x_breaks[1:num_bins], y = y_e3, lty = 3, lwd = 2)#, col = era_tab$color_minflow[era_tab$era==3])
    
    # legend(x="topleft",lwd=2, col = era_tab$color_minflow,
    #        legend = paste0("Era ",era_tab$era,": ",era_yrs_text))
    
    legend(x="topleft",
           col = c(era_tab$color_minflow,"black","black","black"),
           legend = c(paste0("Era ",era_tab$era,": ",era_yrs_text),
                      paste0("Median, Era ",era_tab$era)),
           lwd=c(1,1,1,2,2,2),lty=c(1,1,1,1,2,3))
  }
  
  
}

flow_to_aq_and_stream_fig = function(plot_panel = NA){
  if(is.na(plot_panel)){par(mfrow = c(2,1))}
  
  
  summary_tab = stream_aq_tab
  
  if(is.na(plot_panel) | plot_panel==1){
    # Plots over time
    par(mar = c(5,4,2,5))
    plot(x=NA,y=NA, xlim = c(1,n_sp), #log = "y",
         ylim = range(c(summary_tab$tot_flow_to_stm, summary_tab$tot_flow_to_aq*-1.3)),
         xlab = "Stress Period (Months starting Oct. 1990)",
         ylab = "Sim. water vol. per month (Mm3)", xaxt = "n")
    yr_labels_at = summary_tab$SP[summary_tab$SP %% 60 == (60-8)] #label at Jan of 95, 00, 05, 10, 15
    yr_labels = seq(from=1995, to = 1991+(nrow(summary_tab)/12 - 1), by = 5)
    axis(side = 1, at = yr_labels_at, labels= yr_labels)
    points(summary_tab$SP, summary_tab$tot_flow_to_stm, type = "o", col = "dodgerblue", pch = 20)
    points(summary_tab$SP, summary_tab$tot_flow_to_aq*-1, type = "o", col = "firebrick1", pch = 17) # make neg again
    points(summary_tab$SP, summary_tab$net_flow_to_stm, type = "o", col = "gray20", pch = 18)
    abline(h=pretty(range(c(summary_tab$tot_flow_to_stm, summary_tab$tot_flow_to_aq*-1))),
           v = yr_labels_at, lty = 3, col = "gray")
    abline(h=0)
    legend(x = "bottomleft", ncol = 3, pch=c(20,17,18), cex = .8,
           col = c("dodgerblue","firebrick1","gray20"), bg="white",
           legend = c("to stream", "to aquifer", "net to stream"))
    text(x=3,y=2,labels="A")
  }
  
  # Panel 2
  if(is.na(plot_panel) | plot_panel==2){
    # #Add observed FJ volumes to summary tab
    # fj_obs_svihm_period = fj_flow[fj_flow$Date >= as.Date("1990-10-01") &
    #                                 fj_flow$Date <= as.Date("2018-09-30"),]
    # fj_obs_sp = aggregate(fj_obs_svihm_period$Flow * cfs_to_m3day,
    #                       by = list(floor_date(fj_obs_svihm_period$Date, unit = "month")),
    #                       FUN = sum)
    # summary_tab$fj_obs_Mm3 = fj_obs_sp$x / 10^6
    
    # plot(summary_tab$fj_obs_m3/10^6, summary_tab$fj_Mm3, log = "xy"); abline(a=0,b=1)
    
    summary_tab$season = "Dec-Feb"
    summary_tab$season[summary_tab$SP %% 12 %in% 6:8] = "Mar-May"
    summary_tab$season[summary_tab$SP %% 12 %in% 9:11] = "Jun-Aug"
    summary_tab$season[summary_tab$SP %% 12 %in% 0:2] = "Sep-Nov"
    
    spring_col_ch3 = "chartreuse3"
    summer_col = "gold3" # gold
    fall_col_ch3 = "orangered"
    winter_col = "dodgerblue"
    
    season_color_tab = data.frame(season = c("Dec-Feb","Mar-May", "Jun-Aug","Sep-Nov"),
                                  color = c(winter_col, spring_col_ch3, summer_col,fall_col_ch3),
                                  symbol = c(15,16,17,18))
    
    summary_tab$season_color = season_color_tab$color[match(summary_tab$season, season_color_tab$season)]
    summary_tab$season_symbol = season_color_tab$symbol[match(summary_tab$season, season_color_tab$season)]
    
    # Scatterplots
    x_and_ys = data.frame(t(combn(x = colnames(summary_tab[2:5]), m=2)))
    colnames(x_and_ys)=c("x","y")
    x_and_ys$long_xname = c(rep("Total GW discharge to stream (m3)",3),
                            rep("Total stream leakage to aq. (m3)",2),
                            "Net flux, aquifer to stream (Mm3)")
    x_and_ys$long_yname = c("Total stream leakage to aq. (m3)",
                            "Net flow, aq. to stream (m3)", "FJ flow volume (m3)",
                            "Net flow, aq. to stream (m3)","FJ flow volume (m3)",
                            "Sim. FJ flow vol. / month (Mm3)")
    
    Mm3_lims = c(0,2)
    
    
    # Plot just net flux vs FJ flow
    par(mar = c(5,4,2,5))
    i = 6
    xname = as.character(x_and_ys$x[i])
    yname = as.character(x_and_ys$y[i])
    xname_long = x_and_ys$long_xname[i]
    yname_long = x_and_ys$long_yname[i]
    plot(x=summary_tab[,xname], y= summary_tab[,yname],
         pch = summary_tab$season_symbol, 
         col = alpha(summary_tab$season_color,.4), #col= rgb(0,0,0,.1),
         # xlim = Mm3_lims, #ylim = Mm3_lims,
         xlab = xname_long, ylab = yname_long, 
         log = "y", yaxt = "n")
    abline(v=0)
    grid()
    flow_labels = c(".001","0.1", "1", "10", "100","1,000")
    axis(side = 2, at = 1 * 10^(-2:3), labels = flow_labels, las = 1)
    axis(side = 2, at = 2:9 * 10^(sort(rep(-2:3,9))), tck = -.01, labels=NA)
    legend(x = "bottomright", pch = season_color_tab$symbol, col = season_color_tab$color,
           # legend = paste("SP in",season_color_tab$season))
           legend = season_color_tab$season)
    
    # make 2nd y-axis
    par(new=T)
    plot(x=summary_tab[,xname], y= rep(NA, nrow(summary_tab)), 
         ylim = range(y= summary_tab[,yname])*Mm3month_AugDec_to_cfs, log="y",
         axes=F, bty="n",xlab="",ylab="")
    flow_labels = c("10", "100", "1,000")
    axis(side = 4, at = 1 * 10^(1:3), labels = flow_labels, las = 1)
    axis(side = 4, at = 2:9 * 10^(sort(rep(-1:3,9))), tck = -.01, labels=NA)
    mtext(text = "Approx. Avg. Flow (cfs)",
          side = 4, line = 3.5)
    abline(h=Q_spill, lwd = 2, lty = 2)
    text(x = .75,y = 1.5*Q_spill, labels = "Q spill threshold")
    
    text(x=-0.3,y=1800,labels="B")
  }
    
}



fall_flows_fig = function(){
  # initialize plot
  # par(mar=c(5,4,2,5))
  plot(dsb_tab$year, rep(NA,nrow(dsb_tab)), ylim = c(0.3,800), log = "y",
       xlab = "Year",ylab = "Flow Vol. (Mm3 per month)", yaxt="n")
  grid()
  flow_labels = c("0.1", "1", "10", "100","1,000")
  axis(side = 2, at = 1 * 10^(-1:3), labels = flow_labels, las = 1)
  axis(side = 2, at = 2:9 * 10^(sort(rep(-1:3,9))), tck = -.01, labels=NA)
  #Annotate eras
  arrows(x0=1942, x1=1976,y0=500,y1=500,code=3,angle=90,length=.05)
  arrows(x0=1976, x1=2000,y0=500,y1=500,code=3,angle=90,length=.05)
  arrows(x0=2000, x1=2021,y0=500,y1=500,code=3,angle=90,length=.05)
  text(x=mean(c(1942,1976)), y = 700, label = "Era 1")
  text(x=mean(c(1976,2000)), y = 700, label = "Era 2")
  text(x=mean(c(2000,2021)), y = 700, label = "Era 3")
  
  for(i in 1:nrow(ff_color)){
    points(dsb_tab$year, dsb_tab[,paste0(ff_color$agg_window[i],"_flow")]/10^6,
           ylim = c(0,180), type = "o", pch=18, col = ff_color$color[i])
  }
  points(dsb_tab$year, dsb_tab$min_30day_flow_jul_dec_cal_yr/10^6,
         ylim = c(0,180), type = "o", pch=18, col = rgb(0,0,0,.4))
  
  # make 2nd y-axis
  par(new=T)
  plot(x = dsb_tab$year, rep(NA,nrow(dsb_tab)), 
       ylim = c(0.3,400)*Mm3month_AugDec_to_cfs, log="y",
       axes=F, bty="n",xlab="",ylab="",
       main = "Monthly flow volumes, Aug.-Dec., 1942-2021")
  axis(side = 4, at = 1 * 10^(-1:3), labels = flow_labels, las = 1)
  axis(side = 4, at = 2:9 * 10^(sort(rep(-1:3,9))), tck = -.01, labels=NA)
  mtext(text = "Approx. Avg. Flow (cfs)",
        side = 4, line = 3.5)
  
  legend(x="bottomleft", legend=c(ff_color$agg_window,"Min."), ncol=3,
         # pch=21, pt.bg=ff_color$color)
         pch=18, cex = .9, col=c(ff_color$color,rgb(0,0,0,.4)))
}


fall_min_flow_timing_fig = function(keep_these_cols){
  # par(mar=c(5,4,3,2))
  
  # Plot minimum flow timing over the years
  n_yrs = nrow(dsb_tab) # take out the last year. 2021 doesn't have a full end of calendar year I think.
  plot(x = dsb_tab$year, 
       y = dsb_tab$min_30day_flow_jday, 
       yaxt = "n", pch = 19, type = "o",
       main = "Fall minimum flow timing, 1942-2021",
       xlab = "Year", ylab = "Midpoint of 30-day min. flow window")
  axis_dates = seq.Date(from = as.Date("2001-07-01"), to = as.Date("2002-03-30"), by = "month")
  axis_lab_pts = as.numeric(axis_dates - as.Date("2001-07-01"))
  axis(side = 2, at = axis_lab_pts, labels = format(axis_dates, "%b-%d"))
  abline(h = axis_lab_pts, v = seq(from = 1940, to = 2020, by = 10), 
         col = "darkgray", lty = 3)
  
}


p_spill_fig = function(){
  # initialize plot
  # par(mar=c(5,4,3,5))
  plot(dsb_tab$year, 
       dsb_tab$cum_ppt_on_first_day_qspill,
       xlab = "Year",ylab = "P spill (mm)",
       main = "Cumulative precip. on first day of FJ flow greater than 120 cfs, 1942-2021",
       pch = 19, type = "o")
  grid()
  
  #Annotate eras
  # arrows(x0=1942, x1=1976,y0=500,y1=500,code=3,angle=90,length=.05)
  # arrows(x0=1976, x1=2000,y0=500,y1=500,code=3,angle=90,length=.05)
  # arrows(x0=2000, x1=2021,y0=500,y1=500,code=3,angle=90,length=.05)
  # text(x=mean(c(1942,1976)), y = 700, label = "Era 1")
  # text(x=mean(c(1976,2000)), y = 700, label = "Era 2")
  # text(x=mean(c(2000,2021)), y = 700, label = "Era 3")
  
  # make 2nd y-axis
  par(new=T)
  plot(x = dsb_tab$year, 
       y = dsb_tab$cum_ppt_on_first_day_qspill * mm_to_in, 
       # ylim = c(0.3,400)*Mm3month_AugDec_to_cfs, log="y",
       axes=F, bty="n", xlab="",ylab="")
  axis(side = 4, 
       at = pretty(range(dsb_tab$cum_ppt_on_first_day_qspill, na.rm = T)
                   * mm_to_in) )
  # axis(side = 4, at = 2:9 * 10^(sort(rep(-1:3,9))), tck = -.01, labels=NA)
  mtext(text = "P spill (inches)",
        side = 4, line = 3.5)
  
}


corr_matrix_fig_ch3 = function(keep_these_cols, 
                               return_matrix = T, annotate_with_boxes =T){
  
  sep_flow_col = "orangered"
  min_fall_flow_col = sep_flow_col
  p_spill_col = "goldenrod"
  spring_flow_col = "green4"
  snow_col = "gray40"
  wl_col = "dodgerblue"
  colname_colors = c(min_fall_flow_col, min_fall_flow_col, 
                     spring_flow_col, spring_flow_col,
                     rep(snow_col, length(keep_snow)), 
                     rep(precip_col, length(keep_wx)), 
                     rep(wl_col,length(keep_wells)),
                     et_col, 
                     rep(snow_col, length(keep_snow)), 
                     rep(precip_col, length(keep_wx)),
                     rep(snow_col, length(keep_snow_last)), 
                     rep(precip_col, length(keep_wx_last)), 
                     rep(wl_col,length(keep_wells_last)))
  fftr = dsb_tab[,keep_these_cols]
  #rename fftr columns for fig. legibility
  colnames(fftr)[colnames(fftr)=="cum_ppt_on_first_day_qspill"] = "P_spill"
  colnames(fftr)[colnames(fftr)=="min_fall_flow"] = "V_min, 30 days"#"Q_min"
  colnames(fftr)[colnames(fftr) %in% c("mar_flow","apr_flow")] = c("Mar. flow","Apr. flow")
  colnames(fftr)[colnames(fftr) %in% paste0(keep_snow,"_max_wc_mm")] =
    paste0("Snow ",1:length(keep_snow)," (",keep_snow,")")
  colnames(fftr)[colnames(fftr) %in% paste0(keep_wx,"_oct_apr_mm")] =
    paste0("Precip. ",1:length(keep_wx)," (",keep_wx,")")
  # colnames(fftr)[colnames(fftr) %in% paste0("aprWL_",keep_wells)] =
  #   paste0("Apr. WL ", 1:2," (", keep_wells,")") # stupid well names are too long
  # colnames(fftr)[colnames(fftr) %in% paste0("aprWL_",keep_wells)] =
  #   paste0("Apr. WL Well ", 1:2)
  colnames(fftr)[colnames(fftr) %in% paste0("springWL_",keep_wells)] =
    paste0("Mar-May WL Well ", 1:2)
  colnames(fftr)[colnames(fftr) == "et0_oct_apr"] = "Ref. ET"
  colnames(fftr)[colnames(fftr) %in% paste0(keep_snow,"_jday_of_max")] =
    paste0("Snow timing ",1:length(keep_snow))
  colnames(fftr)[colnames(fftr) %in% paste0(keep_wx,"_jday_of_median")] =
    paste0("Precip. timing ",1:length(keep_wx))
  
  colnames(fftr)[colnames(fftr) %in% paste0(keep_snow_last,"_max_wc_mm_last_year")] =
    paste0("Snow 3 prev. year")
  colnames(fftr)[colnames(fftr) %in% paste0(keep_wx_last,"_oct_apr_mm_last_year")] =
    paste0("Precip. 2 prev. year")
  # colnames(fftr)[colnames(fftr) %in% paste0("aprWL_",keep_wells_last,"_last_year")] =
  #   paste0("Apr. WL 2 prev. year")
  colnames(fftr)[colnames(fftr) %in% paste0("springWL_",keep_wells_last,"_last_year")] =
    paste0("Mar-May WL ",1:length(keep_wells_last), ", prev. year")
  # make giant correlation matrix for flow tabs
  
  
  
  
  
  # round(cor_mat[2,],2) # see values for sep flows corr. coeffs
  
  
  cor_mat = cor(fftr, use = "pairwise.complete.obs")
  # corrplot::corrplot(cor_mat, na.label = "--")
  pt5_x = c(-.5,-.5, .5,.5,-.5)
  pt5_y = c(.5, -.5, -.5, .5, .5)
  n = length(keep_these_cols)
  # Make rectangles to highlight groups. First, find indices
  # sep_index = grep(pattern = "sep_", keep_these_cols)
  min_fall_flow_index = grep(pattern = "min_fall_flow", keep_these_cols)
  p_spill_index = grep(pattern = "cum_ppt_on_first_day_qspill", keep_these_cols)
  mar_flow_index = grep(pattern = "mar_flow", keep_these_cols)
  apr_flow_index = grep(pattern = "apr_flow", keep_these_cols) 
  snow_indices = which(grepl(pattern = "max_wc_mm",  keep_these_cols) &
                         !grepl(pattern = "last", keep_these_cols))
  precip_indices = which(grepl(pattern = "oct_apr_mm",  keep_these_cols) &
                           !grepl(pattern = "last", keep_these_cols))
  et_index = grep(pattern = "et0", keep_these_cols)
  # wl_indices = which(grepl(pattern = "aprWL",  keep_these_cols) &
  #                      !grepl(pattern = "last", keep_these_cols))
  wl_indices = which(grepl(pattern = "springWL",  keep_these_cols) &
                       !grepl(pattern = "last", keep_these_cols))
  four_cat_indices = range(c(mar_flow_index, apr_flow_index, 
                             snow_indices, precip_indices, wl_indices))
  snowjd_indices = grep(pattern = "jday_of_max", keep_these_cols)
  precipjd_indices = grep(pattern = "jday_of_median", keep_these_cols)
  last_yr_indices = grep(pattern="last",keep_these_cols)
  
  
  corrplot::corrplot(cor_mat, na.label = "--", diag=T, tl.col = colname_colors)
  
  if(annotate_with_boxes ==T){
    # highlight max snow depth
    # manually make these weird coordinates for the polygons
    min_fall_flow_corners_x1 = c(rep(min_fall_flow_index,2), rep(p_spill_index,2), 
                                 min_fall_flow_index) + pt5_x
    min_fall_flow_corners_y1 = n + 1 - c(min(four_cat_indices), rep(max(four_cat_indices),2),
                                         rep(min(four_cat_indices),2)) + pt5_y
    min_fall_flow_corners_x2 = c(rep(min(four_cat_indices),2), rep(max(four_cat_indices),2), 
                                 min(four_cat_indices)) + pt5_x
    min_fall_flow_corners_y2 = n + 1 - c(min_fall_flow_index, rep(p_spill_index,2), 
                                         rep(min_fall_flow_index,2)) + pt5_y
    snow_corners_x = c(rep(min(snow_indices),2), rep(max(snow_indices),2),min(snow_indices)) + pt5_x
    snow_corners_y = n + 1 - c(min(snow_indices), rep(max(snow_indices),2), rep(min(snow_indices),2)) + pt5_y
    precip_corners_x = c(rep(min(precip_indices),2), rep(max(precip_indices),2),min(precip_indices)) + pt5_x
    precip_corners_y = n + 1 - c(min(precip_indices), rep(max(precip_indices),2), rep(min(precip_indices),2)) + pt5_y
    wl_corners_x = c(rep(min(wl_indices),2), rep(max(wl_indices),2),min(wl_indices)) + pt5_x
    wl_corners_y = n + 1 - c(min(wl_indices), rep(max(wl_indices),2), rep(min(wl_indices),2)) + pt5_y
    snowjd_corners_x = c(rep(min(snowjd_indices),2), rep(max(snowjd_indices),2),min(snowjd_indices)) + pt5_x
    snowjd_corners_y = n + 1 - c(min(snowjd_indices), rep(max(snowjd_indices),2), rep(min(snowjd_indices),2)) + pt5_y
    precipjd_corners_x = c(rep(min(precipjd_indices),2), rep(max(precipjd_indices),2),min(precipjd_indices)) + pt5_x
    precipjd_corners_y = n + 1 - c(min(precipjd_indices), rep(max(precipjd_indices),2), rep(min(precipjd_indices),2)) + pt5_y
    p_spill_corners_x1 = c(rep(min_fall_flow_index,2), rep(p_spill_index,2), 
                           min_fall_flow_index) + pt5_x
    p_spill_corners_y1 = n + 1 - c(min(last_yr_indices), rep(max(last_yr_indices),2),
                                   rep(min(last_yr_indices),2)) + pt5_y
    p_spill_corners_x2 = c(rep(min(last_yr_indices),2), rep(max(last_yr_indices),2), 
                           min(last_yr_indices)) + pt5_x
    p_spill_corners_y2 = n + 1 - c(min_fall_flow_index, rep(p_spill_index,2), 
                                   rep(min_fall_flow_index,2))  + pt5_y
    

    same_cat_color = "goldenrod"
    same_cat_timing_color = "gray40"
    polygon(x=snow_corners_x, y = snow_corners_y, lwd = 4, border = snow_col)
    polygon(x=precip_corners_x, y = precip_corners_y, lwd = 4, border = precip_col)
    polygon(x=wl_corners_x, y = wl_corners_y, lwd = 4, border = wl_col)
    polygon(x=snowjd_corners_x, y = snowjd_corners_y, lwd = 4, border = snow_col)
    polygon(x=precipjd_corners_x, y = precipjd_corners_y, lwd = 4, border = precip_col)
    polygon(x=min_fall_flow_corners_x1, y = min_fall_flow_corners_y1, lwd = 4, border = min_fall_flow_col)
    polygon(x=min_fall_flow_corners_x2, y = min_fall_flow_corners_y2, lwd = 4, border = min_fall_flow_col)
    polygon(x=p_spill_corners_x1, y = p_spill_corners_y1, lwd = 4, border = p_spill_col)
    polygon(x=p_spill_corners_x2, y = p_spill_corners_y2, lwd = 4, border = p_spill_col)
  }


  if(return_matrix == T){return(cor_mat)}
}


get_high_corr_wells = function(show_map = F,show_corr_plots = F,
                               threshold_n_obs = 10, 
                               corr_wells = NA,
                               threshold_aprwl_sepflow_corr = .5){
  
  # # March WLs
  # cor_tabs_mar = gw_vs_fall_flows_data_exp(fall_flow_window_tab = ff_color,
  #                                     spring_months = 3,
  #                                      save_well_by_well_pdf = F,
  #                                      show_well_by_well_plots = F,
  #                                      show_corr_map_plots = T)
  # cor_tab_mar = cor_tabs_mar[[1]]
  # m_tab_mar = cor_tabs_mar[[2]]
  # apr_wl_min_fall_flow = cor_tab_mar[,c("well_code", "sep_cor", "oct_cor","nov_cor")]
  # gw_flow = merge(apr_wl_min_fall_flow, m_tab_mar[,c("well_code", "sep_slope",
  #                              "oct_slope", "nov_slope")], by = "well_code")
  # # Pick thresholds to ID high-value predictive wells
  # highlight_wells = gw_flow$well_code[gw_flow$sep_cor>0.5 &
  #                                       gw_flow$oct_cor>0.5 &
  #                                       !is.na(gw_flow$sep_cor)]
  # plot(wells_sp[wells_sp$well_code %in% highlight_wells,], add=T, pch = 5,
  #      bg=NA, lwd=2, cex = 1.2, col = "red")
  
  
  # April WLs
  cor_tabs_apr = gw_vs_fall_flows_data_exp(fall_flow_window_tab = ff_color,
                                           spring_months = 4,
                                           save_well_by_well_pdf = F,
                                           show_well_by_well_plots = F,
                                           show_corr_map_plots = show_map)
  cor_tab_apr = cor_tabs_apr[[1]]
  m_tab_apr = cor_tabs_apr[[2]]
  n_tab_apr = cor_tabs_apr[[3]]
  apr_wl_min_fall_flow = cor_tab_apr[,c("well_code", "sep_cor", "oct_cor","nov_cor")]
  gw_flow = merge(apr_wl_min_fall_flow, m_tab_apr[,c("well_code", "sep_slope",
                                                     "oct_slope", "nov_slope")], by = "well_code")
  gw_flow = merge(gw_flow, n_tab_apr[,c("well_code", "sep_n_obs",
                                        "oct_n_obs", "nov_n_obs")], by = "well_code")
  # Pick thresholds to ID high-value predictive wells
  highlight_wells = gw_flow$well_code[gw_flow$sep_cor> threshold_aprwl_sepflow_corr &
                                        # gw_flow$oct_cor>0.5 &
                                        gw_flow$sep_n_obs >= threshold_n_obs & 
                                        !is.na(gw_flow$sep_n_obs)
                                      # ,!is.na(gw_flow$sep_cor)
                                      ]
  if(show_map==T){
    # plot(wells_sp[wells_sp$well_code %in% highlight_wells,], add=T, pch = 5,
    #      bg=NA, lwd=2, cex = 2, col = "black")
    if(sum(!is.na(corr_wells))>0){
      plot(wells_sp$geometry[wells_sp$well_code %in% corr_wells], add=T, pch = 0,
           bg=NA, lwd=2, cex = 2.5, col = "red")
      plot(riv$geometry, add=T, lwd = 3, col = color_river)
      north(xy="right",type=1)
      sbar(xy="topright", d=10^4, below= "km", label = c(0,5,10))
      legend(x="bottomright",lwd=c(1,2),col=c("black","blue"),
             legend=c("Basin boundary","Scott River"))
    }
  }
  
  if(show_corr_plots==T){
    
    plot(gw_flow$sep_slope, gw_flow$sep_cor, ylim = c(-1,1), #xlim = c(-60,60),
         main = "Strength of relationships between Sept. \n FJ Flow vol. pred. by Apr. GW", pch=19,
         xlab = "slope of line of best fit (Mm3 / ft)",
         ylab = "Corr. coeff. of relationship between WL record and FJ Sept. flows")
    grid()
    
  }
  
  
  # # May WLs
  # cor_tabs_may = gw_vs_fall_flows_data_exp(fall_flow_window_tab = ff_color,
  #                                     spring_months = 5,
  #                                      save_well_by_well_pdf = F,
  #                                      show_well_by_well_plots = F,
  #                                      show_corr_map_plots = T)
  # cor_tab_may = cor_tabs_may[[1]]
  # m_tab_may = cor_tabs_may[[2]]
  # apr_wl_min_fall_flow = cor_tab_may[,c("well_code", "sep_cor", "oct_cor","nov_cor")]
  # gw_flow = merge(apr_wl_min_fall_flow, m_tab_may[,c("well_code", "sep_slope",
  #                              "oct_slope", "nov_slope")], by = "well_code")
  # # Pick thresholds to ID high-value predictive wells
  # highlight_wells = gw_flow$well_code[gw_flow$sep_cor>0.5 &
  #                                       gw_flow$oct_cor>0.5 &
  #                                       !is.na(gw_flow$sep_cor)]
  # plot(wells_sp[wells_sp$well_code %in% highlight_wells,], add=T, pch = 5,
  #      bg=NA, lwd=2, cex = 1.2, col = "red")
  
  
  # # WLs in April and May (bigger sample size, but produced different set of highlighted wells compared to April-only)
  # cor_tabs_apr_may = gw_vs_fall_flows_data_exp(fall_flow_window_tab = ff_color,
  #                                     spring_months = 4:5,
  #                                      save_well_by_well_pdf = F,
  #                                      show_well_by_well_plots = F,
  #                                      show_corr_map_plots = T)
  # cor_tab_apr_may = cor_tabs_apr_may[[1]]
  # m_tab_apr_may = cor_tabs_apr_may[[2]]
  # apr_wl_min_fall_flow = cor_tab_apr_may[,c("well_code", "sep_cor", "oct_cor","nov_cor")]
  # gw_flow = merge(apr_wl_min_fall_flow, m_tab_apr_may[,c("well_code", "sep_slope",
  #                              "oct_slope", "nov_slope")], by = "well_code")
  # # Pick thresholds to ID high-value predictive wells
  # highlight_wells = gw_flow$well_code[gw_flow$sep_cor>0.5 &
  #                                       gw_flow$oct_cor>0.5 &
  #                                       !is.na(gw_flow$sep_cor)]
  # plot(wells_sp[wells_sp$well_code %in% highlight_wells,], add=T, pch = 5,
  #      bg=NA, lwd=2, cex = 1.2, col = "red")
  
  return(highlight_wells)
  
}



one_predictor_model_plots = function(tab_for_glm, panel_arrange = c(3,2),
                                     annotate_plots = T,
                                     return_diag_tab=F,
                                     make_plots=T){
  
  #initialize model diagnostics table
  preds = c("SWJ_max_wc_mm","USC00043182_oct_apr_mm",
            "SWJ_jday_of_max","springWL_415635N1228315W001",
            "et0_oct_apr","mar_flow")
  diag_tab = data.frame(pred=preds,logLike=NA,AICc=NA,loocv=NA)
  par(mfrow = panel_arrange)
  ylim_mult = 1.1
  
  # Calculate LOOCVs and plot pred v obs
  
  # A. LOOCV of best single predictor, SWJ snow max
  pred = "SWJ_max_wc_mm"
  tab_for_glm1a = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
                                       tab_for_glm[,pred]),]
  f1a = paste("min_fall_flow ~ ", pred)
  glm_1a = glm(f1a, data = tab_for_glm1a)
  loocv_1a = cv.glm(tab_for_glm1a, glm_1a)
  #attach lm results 
  diag_tab$logLike[diag_tab$pred==pred] = logLik(glm_1a)
  diag_tab$AICc[diag_tab$pred==pred] = AICc(glm_1a)
  diag_tab$loocv[diag_tab$pred==pred] = loocv_1a$delta[1]
  #Generate plot
  obs = tab_for_glm1a$min_fall_flow
  pred = predict.glm(object = glm_1a, newdata=tab_for_glm1a)
  # Plot observed vs predicted for SWJ station
  if(make_plots==T){par(mar=c(5,5,3,2))
  plot(x=obs, y = pred, col = tab_for_glm1a$era_color_minflow, pch=19, 
       asp=1, ylim = c(range(pred)[1], range(pred)[2]*ylim_mult),
       main = "Snow maximum",
       xlab = "Observed min. fall. flow volume (Mm3/ 30 days)",
       ylab = "Min. fall flow volume (Mm3 / 30 d) \n predicted from SWJ snow max")
  grid()
  abline(a=0,b=1)
  if(annotate_plots==T){
    text(x=3,y=0,label="1:1 line")
    text(x=8,y=7,label=paste("LOOCV Error: ", round(loocv_1a$delta[1],1)), pos=4)
  }
  era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
  legend(x="bottomright", col = era_tab$color_minflow, 
         legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19)}
  
  # B. LOOCV of next best single predictor, rainy station
  pred = "USC00043182_oct_apr_mm"
  tab_for_glm1b = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
                                       tab_for_glm[,pred]),]
  f1b = paste("min_fall_flow ~ ", pred)
  glm_1b = glm(f1b, data = tab_for_glm1b)
  loocv_1b = cv.glm(tab_for_glm1b, glm_1b)
  #attach lm results 
  diag_tab$logLike[diag_tab$pred==pred] = logLik(glm_1b)
  diag_tab$AICc[diag_tab$pred==pred] = AICc(glm_1b)
  diag_tab$loocv[diag_tab$pred==pred] = loocv_1b$delta[1]
  #Generate plot
  obs = tab_for_glm1b$min_fall_flow
  pred = predict.glm(object = glm_1b, newdata=tab_for_glm1b)
  # Plot observed vs predicted for SWJ station
  if(make_plots==T){par(mar=c(5,5,3,2))
    plot(x=obs, y = pred, col = tab_for_glm1a$era_color_minflow, pch=19, 
         asp=1, ylim = c(range(pred)[1], range(pred)[2]*ylim_mult),
         main = "Oct.-Apr. precip.",
         xlab = "Observed min. fall. flow volume (Mm3)",
         ylab = "Min. fall flow volume (Mm3 / 30 d) \n predicted from FJ Oct-Apr precip.")
    grid()
    abline(a=0,b=1)
    if(annotate_plots==T){
      text(x=10,y=7.5,label="1:1 line")
      text(x=0,y=7.5,label=paste("LOOCV Error: ", round(loocv_1b$delta[1],1)), pos=4)
    }
    era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
    legend(x="bottomright", col = era_tab$color_minflow, 
           legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19)}
  
  # A2. LOOCV of best single predictor, SWJ snow max timing
  pred = "SWJ_jday_of_max"
  tab_for_glm1aa = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
                                        tab_for_glm[,pred]),]
  f1aa = paste("min_fall_flow ~ ", pred)
  glm_1aa = glm(f1aa, data = tab_for_glm1aa)
  loocv_1aa = cv.glm(tab_for_glm1aa, glm_1aa)
  #attach lm results 
  diag_tab$logLike[diag_tab$pred==pred] = logLik(glm_1aa)
  diag_tab$AICc[diag_tab$pred==pred] = AICc(glm_1aa)
  diag_tab$loocv[diag_tab$pred==pred] = loocv_1aa$delta[1]
  #Generate plot
  obs = tab_for_glm1aa$min_fall_flow
  pred = predict.glm(object = glm_1aa, newdata=tab_for_glm1aa)
  # Plot observed vs predicted for SWJ station
  if(make_plots==T){par(mar=c(5,5,3,2))
    plot(x=obs, y = pred, col = tab_for_glm1aa$era_color_minflow, pch=19, 
         asp=1, ylim = c(range(pred)[1], range(pred)[2]*ylim_mult),
         main = "Snow maximum timing",
         xlab = "Observed min. fall. flow volume (Mm3)",
         ylab = "Min. fall flow volume (Mm3 / 30 d) \n predicted from SWJ snow max timing")
    grid()
    abline(a=0,b=1)
    if(annotate_plots==T){
      text(x=4,y=6,label="1:1 line")
      text(x=7,y=6,label=paste("LOOCV Error: ", round(loocv_1aa$delta[1],1)), pos=4)
    }
    era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
    legend(x="bottomright", col = era_tab$color_minflow, 
           legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19) } 
  
  # C. LOOCV of next best single predictor, water level in a well
  pred = "springWL_415635N1228315W001"#"aprWL_415635N1228315W001"
  tab_for_glm1c = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
                                       tab_for_glm[,pred]),]
  f1c = paste("min_fall_flow ~ ", pred)
  glm_1c = glm(f1c, data = tab_for_glm1c)
  loocv_1c = cv.glm(tab_for_glm1c, glm_1c)
  #attach lm results 
  diag_tab$logLike[diag_tab$pred==pred] = logLik(glm_1c)
  diag_tab$AICc[diag_tab$pred==pred] = AICc(glm_1c)
  diag_tab$loocv[diag_tab$pred==pred] = loocv_1c$delta[1]
  #Generate plot
  obs = tab_for_glm1c$min_fall_flow
  pred = predict.glm(object = glm_1c, newdata=tab_for_glm1c)
  # Plot observed vs predicted for SWJ station
  if(make_plots==T){par(mar=c(5,5,3,2))
    plot(x=obs, y = pred, col = tab_for_glm1c$era_color_minflow, pch=19, 
         asp=1, ylim = c(range(pred)[1], range(pred)[2]*ylim_mult),
         main = "Mar-May WLs",#"April WLs",
         xlab = "Observed min. fall. flow volume (Mm3)",
         ylab = "Min. fall flow volume (Mm3 / 30 d) predicted \n from well 415635N1228315W001")
    grid()
    abline(a=0,b=1)
    if(annotate_plots==T){
      text(x=4,y=5.5,label="1:1 line")
      text(x=6,y=5.5,label=paste("LOOCV Error: ", round(loocv_1c$delta[1],1)), pos=4)
    }
    era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
    legend(x="bottomright", col = era_tab$color_minflow, 
           legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19)}
  
  
  # D. LOOCV of next best single predictor, ET0
  pred = "et0_oct_apr"
  tab_for_glm1d = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
                                       tab_for_glm[,pred]),]
  f1d = paste("min_fall_flow ~ ", pred)
  glm_1d = glm(f1d, data = tab_for_glm1d)
  loocv_1d = cv.glm(tab_for_glm1d, glm_1d)
  #attach lm results 
  diag_tab$logLike[diag_tab$pred==pred] = logLik(glm_1d)
  diag_tab$AICc[diag_tab$pred==pred] = AICc(glm_1d)
  diag_tab$loocv[diag_tab$pred==pred] = loocv_1d$delta[1]
  #Generate plot
  obs = tab_for_glm1c$min_fall_flow
  obs = tab_for_glm1d$min_fall_flow
  pred = predict.glm(object = glm_1d, newdata=tab_for_glm1d)
  # Plot observed vs predicted for SWJ station
  if(make_plots==T){par(mar=c(5,5,3,2))
    plot(x=obs, y = pred, col = tab_for_glm1d$era_color_minflow, pch=19,
         asp=1, ylim = c(range(pred)[1], range(pred)[2]*ylim_mult),
         main = "Reference ET",
         xlab = "Observed min. fall. flow volume (Mm3)",
         ylab = "Min. fall flow volume (Mm3 / 30 d) \n from ET0")
    grid()
    abline(a=0,b=1)
    if(annotate_plots==T){
      text(x=1.5,y=.5,label="1:1 line")
      text(x=-0.2,y=2.9,label=paste("LOOCV Error: ", round(loocv_1d$delta[1],2)), pos=4)
    }
    era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
    legend(x="bottomright", col = era_tab$color_minflow, 
           legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19)}
  
  
  # E. LOOCV of next best single predictor, march flows
  pred = "mar_flow"
  tab_for_glm1e = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
                                       tab_for_glm[,pred]),]
  f1e = paste("min_fall_flow ~ ", pred)
  glm_1e = glm(f1e, data = tab_for_glm1e)
  loocv_1e = cv.glm(tab_for_glm1e, glm_1e)
  #attach lm results 
  diag_tab$logLike[diag_tab$pred==pred] = logLik(glm_1e)
  diag_tab$AICc[diag_tab$pred==pred] = AICc(glm_1e)
  diag_tab$loocv[diag_tab$pred==pred] = loocv_1e$delta[1]
  #Generate plot
  obs = tab_for_glm1e$min_fall_flow
  pred = predict.glm(object = glm_1e, newdata=tab_for_glm1e)
  # Plot observed vs predicted for SWJ station
  if(make_plots==T){par(mar=c(5,5,3,2))
    plot(x=obs, y = pred, col = tab_for_glm1e$era_color_minflow, pch=19, 
         asp=1, ylim = c(range(pred)[1], range(pred)[2]*ylim_mult),
         main = "March flow vol.",
         xlab = "Observed min. fall. flow volume (Mm3)",
         ylab = "Min. fall flow volume (Mm3 / 30 d) \n from March flow vol.")
    grid()
    abline(a=0,b=1)
    if(annotate_plots==T){
      text(x=2,y=0.5,label="1:1 line")
      text(x=0,y=7,label=paste("LOOCV Error: ", round(loocv_1e$delta[1],1)), pos=4)
    }
    era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
    legend(x="bottomright", col = era_tab$color_minflow, 
           legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19)}
  
  if(return_diag_tab==T){return(diag_tab)}
}

two_predictor_model_plots = function(tab_for_glm, annotate_plots = T){
  par(mfrow = c(2,2))
  
  # A. LOOCV of two best predictors, SWJ snow max + FJ rainfall
  pred1 = "SWJ_max_wc_mm"; pred2 = "USC00043182_oct_apr_mm"
  tab_for_glm2a = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
                                       tab_for_glm[,pred1] *
                                       tab_for_glm[,pred2]),]
  f2a = paste(c("min_fall_flow", paste(c(pred1, pred2), collapse = " + ")), collapse = " ~ ")
  glm_2a = glm(f2a, data = tab_for_glm2a)
  loocv_2a = cv.glm(tab_for_glm2a, glm_2a)
  obs = tab_for_glm2a$min_fall_flow
  pred = predict.glm(object = glm_2a, newdata=tab_for_glm2a)
  # Plot observed vs predicted for SWJ station
  par(mar=c(5,5,3,2))
  plot(x=obs, y = pred, col = tab_for_glm2a$era_color, pch=19, asp=1,
       main = "Snow max. + Oct-Apr precip.",
       xlab = "Observed min. fall. flow volume (Mm3)",
       ylab = "Min. fall flow volume (Mm3 / 30 d) predicted \n from SWJ snow and FJ Oct-Apr precip.")
  grid()
  abline(a=0,b=1)
  if(annotate_plots==T){
    text(x=11.5,y=8,label="1:1 line")
    text(x=0,y=10,label=paste("LOOCV Error: ", round(loocv_2a$delta[1],2)), pos=4)
  }
  era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
  legend(x="bottomright", col = era_tab$color_minflow, 
         legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19)
  
  
  # B. LOOCV of two predictors, SWJ snow max + SWJ snow timing
  pred1 = "SWJ_max_wc_mm"; pred2 = "SWJ_jday_of_max"
  tab_for_glm2b = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
                                       tab_for_glm[,pred1] *
                                       tab_for_glm[,pred2]),]
  f2b = paste(c("min_fall_flow", paste(c(pred1, pred2), collapse = " + ")), collapse = " ~ ")
  glm_2b = glm(f2b, data = tab_for_glm2b)
  loocv_2b = cv.glm(tab_for_glm2b, glm_2b)
  obs = tab_for_glm2b$min_fall_flow
  pred = predict.glm(object = glm_2b, newdata=tab_for_glm2b)
  # Plot observed vs predicted for SWJ station
  par(mar=c(5,5,3,2))
  plot(x=obs, y = pred, col = tab_for_glm2b$era_color, pch=19, asp=1,
       main = "Snow max. + timing",
       xlab = "Observed min. fall. flow volume (Mm3)",
       ylab = "Min. fall flow volume (Mm3 / 30 d) predicted \n from SWJ snow max and timing ")
  grid()
  abline(a=0,b=1)
  if(annotate_plots==T){
    text(x=11.5,y=8,label="1:1 line")
    text(x=0,y=10,label=paste("LOOCV Error: ", round(loocv_2b$delta[1],2)), pos=4)
  }
  era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
  legend(x="bottomright", col = era_tab$color_minflow, 
         legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19)
  
  
  
  # # C. LOOCV of two predictors, SWJ snow max * SWJ snow timing
  # pred1 = "SWJ_max_wc_mm"; pred2 = "SWJ_jday_of_max"
  # tab_for_glm2c = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
  #                                      tab_for_glm[,pred1] *
  #                                      tab_for_glm[,pred2]),]
  # f2c = paste(c("min_fall_flow", paste(c(pred1, pred2), collapse = " * ")), collapse = " ~ ")
  # glm_2c = glm(f2c, data = tab_for_glm2c)
  # loocv_2c = cv.glm(tab_for_glm2c, glm_2c)
  # obs = tab_for_glm2c$min_fall_flow
  # pred = predict.glm(object = glm_2c, newdata=tab_for_glm2c)
  # # Plot observed vs predicted for SWJ station
  # par(mar=c(5,5,3,2))
  # plot(x=obs, y = pred, col = tab_for_glm2c$era_color, pch=19, asp=1,
  #      main = "Snow max. depth * timing",
  #      xlab = "Observed min. fall. flow volume (Mm3)",
  #      ylab = "Min. fall flow volume (Mm3 / 30 d) predicted from \n SWJ snow depth * timing")
  # grid()
  # abline(a=0,b=1)
  # text(x=9.5,y=8,label="1:1 line")
  # text(x=5,y=0,label=paste("LOOCV Error: ", round(loocv_2c$delta[1],2)), pos=4)
  # era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
  # legend(x="bottomright", col = era_tab$color_minflow, 
  #        legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19)
  
  
  # # D. LOOCV of three predictors, SWJ snow max * SWJ snow timing + rainfall
  # pred1 = "SWJ_max_wc_mm"; pred2 = "SWJ_jday_of_max"; pred3 = "USC00043182_oct_apr_mm"
  # tab_for_glm3a = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
  #                                      tab_for_glm[,pred1] *
  #                                      tab_for_glm[,pred2] *
  #                                      tab_for_glm[,pred3]),]
  # # f2c = paste(c("min_fall_flow", paste(c(pred1, pred2), collapse = " * ")), collapse = " ~ ")
  # f3a = paste("min_fall_flow ~ 0+",pred1, "*",pred2,"+",pred3)
  # glm_3a = glm(f3a, data = tab_for_glm3a)
  # loocv_3a = cv.glm(tab_for_glm3a, glm_3a)
  # obs = tab_for_glm3a$min_fall_flow
  # pred = predict.glm(object = glm_3a, newdata=tab_for_glm3a)
  # # Plot observed vs predicted for SWJ station
  # par(mar=c(5,5,3,2))
  # plot(x=obs, y = pred, col = tab_for_glm3a$era_color, pch=19, asp=1,
  #      main = "Snow max. * timing + Oct-Apr precip.",
  #      xlab = "Observed min. fall. flow volume (Mm3)",
  #      ylab = "Min. fall flow volume (Mm3 / 30 d) predicted from SWJ \n snow depth * timing + FJ Oct-Apr precip")
  # grid()
  # abline(a=0,b=1)
  # text(x=9.5,y=8,label="1:1 line")
  # text(x=5,y=0,label=paste("LOOCV Error: ", round(loocv_3a$delta[1],2)), pos=4)
  # era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
  # legend(x="bottomright", col = era_tab$color_minflow, 
  #        legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19)
  
  
  # E. LOOCV of snow + snow of year - 1
  
  pred1 = "SWJ_max_wc_mm"; pred2 = "SWJ_max_wc_mm_last_year" #; pred3 = "USC00043182_oct_apr_mm"
  tab_for_glm2d = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
                                       tab_for_glm[,pred1] *
                                       tab_for_glm[,pred2]),]
  # f2c = paste(c("min_fall_flow", paste(c(pred1, pred2), collapse = " * ")), collapse = " ~ ")
  f2d = paste("min_fall_flow ~ ",pred1, "+",pred2)
  glm_2d = glm(f2d, data = tab_for_glm2d)
  loocv_2d = cv.glm(tab_for_glm2d, glm_2d)
  obs = tab_for_glm2d$min_fall_flow
  pred = predict.glm(object = glm_2d, newdata=tab_for_glm2d)
  # Plot observed vs predicted for SWJ station
  par(mar=c(5,5,3,2))
  plot(x=obs, y = pred, col = tab_for_glm2d$era_color, pch=19, asp=1,
       main = "Snow max. + Snow max. last year",
       xlab = "Observed min. fall. flow volume (Mm3)",
       ylab = "Min. fall flow volume (Mm3 / 30 d) predicted from \n SWJ snow depth this year + last year")
  grid()
  abline(a=0,b=1)
  if(annotate_plots==T){
    text(x=11.5,y=8,label="1:1 line")
    text(x=0,y=10,label=paste("LOOCV Error: ", round(loocv_2d$delta[1],2)), pos=4)
  }
  era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
  legend(x="bottomright", col = era_tab$color_minflow, 
         legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19)
  
  
  # F. LOOCV of rainfall + rainfall of year - 1
  pred1 = "USC00043182_oct_apr_mm"; pred2 = "USC00043182_oct_apr_mm_last_year" #; pred3 = "USC00043182_oct_apr_mm"
  tab_for_glm2e = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
                                       tab_for_glm[,pred1] *
                                       tab_for_glm[,pred2]),]
  # f2c = paste(c("min_fall_flow", paste(c(pred1, pred2), collapse = " * ")), collapse = " ~ ")
  f2e = paste("min_fall_flow ~ ",pred1, "+",pred2)
  glm_2e = glm(f2e, data = tab_for_glm2e)
  loocv_2e = cv.glm(tab_for_glm2e, glm_2e)
  obs = tab_for_glm2e$min_fall_flow
  pred = predict.glm(object = glm_2e, newdata=tab_for_glm2e)
  # Plot observed vs predicted for SWJ station
  par(mar=c(5,5,3,2))
  plot(x=obs, y = pred, col = tab_for_glm2e$era_color, pch=19, asp=1,
       main = "Precip. + Precip. last year",
       xlab = "Observed min. fall. flow volume (Mm3)",
       ylab = "Min. fall flow volume (Mm3 / 30 d) predicted from \n SWJ snow depth this year + last year")
  grid()
  abline(a=0,b=1)
  if(annotate_plots==T){
    text(x=11.5,y=8,label="1:1 line")
    text(x=0,y=10,label=paste("LOOCV Error: ", round(loocv_2e$delta[1],2)), pos=4)
  }
  era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
  legend(x="bottomright", col = era_tab$color_minflow, 
         legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19)
}


one_pred_p_spill = function(pred1, tab_for_glm_p_spill, annotate_plots = T,
                            make_plot = F, return_lm_results = T,
                            plot_title = NA, yaxis_label=NA){
  
  tab_for_glm1g = tab_for_glm_p_spill[!is.na(tab_for_glm_p_spill$cum_ppt_on_first_day_qspill * 
                                               tab_for_glm_p_spill[,pred1]),]
  
  if(nrow(tab_for_glm1g) >0){
    f1g = paste("cum_ppt_on_first_day_qspill ~ ", pred1)#0+", pred1)
    glm_1g = glm(f1g, data = tab_for_glm1g)
    loocv_1g = cv.glm(tab_for_glm1g, glm_1g)
    
    # Pred and obs
    obs = tab_for_glm1g$cum_ppt_on_first_day_qspill
    pred = predict.glm(object = glm_1g, newdata=tab_for_glm1g)
    rmse = sqrt(mean((pred-obs)^2))
    
    # return values
    lm_1g = lm(formula = f1g, data = dsb_tab)
    lm_results = data.frame(pred1 = pred1, 
                            p_val = summary(lm_1g)$coefficients[1,4],
                            r_square = summary(lm_1g)$r.squared,
                            adj_r_square = summary(lm_1g)$adj.r.squared,
                            f_stat = summary(lm_1g)$fstatistic[1],
                            AICc = AICc(lm_1g),
                            loocv = loocv_1g$delta[1],
                            rmse = rmse,
                            n_samp = nrow(tab_for_glm1g))
    
    if(make_plot==T){
      # Plot observed vs predicted for SWJ station
      par(mar=c(5,5,3,2))
      plot(x=obs, y = pred, col = tab_for_glm1g$era_color_pspill, pch=19, asp=1,
           main = plot_title,
           xlim = c(-10,150), ylim=c(-10,150),
           # xlab = "obs", ylab = pred1,
           xlab = "Observed P spill (mm)",
           ylab = paste("P spill (mm) predicted\n using", yaxis_label)
           # ylab = "Min. fall flow volume (Mm3 / 30 d) predicted from \n SWJ snow depth this year + last year"
      )
      grid()
      abline(a=0,b=1)
      if(annotate_plots == T){
        text(x=160,y=130,label="1:1 line")
        text(x=80,y=-20,label=paste("LOOCV Error: ", round(loocv_1g$delta[1])), pos=3)
      }
      era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
      legend(x="topleft", col = era_tab$color_pspill, 
             legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19)
      
    }
    if(return_lm_results==T){return(lm_results)}
  }
  
}




two_pred_p_spill = function(pred1, pred2, tab_for_glm_p_spill, annotate_plots = T,
                            make_plot = F, return_lm_results = T,
                            plot_title = NA, yaxis_label = NA){
  
  tab_for_glm2f = tab_for_glm_p_spill[!is.na(tab_for_glm_p_spill$cum_ppt_on_first_day_qspill * 
                                               tab_for_glm_p_spill[,pred1] *
                                               tab_for_glm_p_spill[,pred2]),]
  
  if(nrow(tab_for_glm2f) >0){
    # f2c = paste(c("min_fall_flow", paste(c(pred1, pred2), collapse = " * ")), collapse = " ~ ")
    f2f = paste("cum_ppt_on_first_day_qspill ~ ",pred1, "+",pred2)
    glm_2f = glm(f2f, data = tab_for_glm2f)
    loocv_2f = cv.glm(tab_for_glm2f, glm_2f)
    
    # Pred and observed      
    obs = tab_for_glm2f$cum_ppt_on_first_day_qspill
    pred = predict.glm(object = glm_2f, newdata=tab_for_glm2f)
    rmse = sqrt(mean((pred-obs)^2))
    
    # return values
    lm_2f = lm(formula = f2f, data = dsb_tab)
    lm_results = data.frame(pred1 = pred1, pred2 = pred2, 
                            p_val1 = summary(lm_2f)$coefficients[1,4],
                            p_val2 = summary(lm_2f)$coefficients[2,4],
                            r_square = summary(lm_2f)$r.squared,
                            adj_r_square = summary(lm_2f)$adj.r.squared,
                            f_stat = summary(lm_2f)$fstatistic[1],
                            AICc = AICc(lm_2f),
                            loocv = loocv_2f$delta[1],
                            rmse = rmse,
                            n_samp = nrow(tab_for_glm2f))
    
    if(make_plot==T){
      # Plot observed vs predicted for SWJ station
      par(mar=c(5,5,3,2))
      plot(x=obs, y = pred, col = tab_for_glm2f$era_color_pspill, pch=19, asp=1,
           main = plot_title,
           xlim = c(-10,150), ylim=c(-10,150),
           # xlab = "obs", ylab = paste(pred1, "+ \n", pred2),
           xlab = "Observed P spill",
           ylab = paste("P spill predicted using \n", yaxis_label)
           # ylab = "Min. fall flow volume (Mm3 / 30 d) predicted from \n SWJ snow depth this year + last year"
      )
      grid()
      abline(a=0,b=1)
      if(annotate_plots == T){
        text(x=160,y=130,label="1:1 line")
        text(x=80,y=-20,label=paste("LOOCV Error: ", round(loocv_2f$delta[1])), pos=3)
      }
      era_yrs_text = c("1942-1976", "1977-2000","2001-2021")
      legend(x="topleft", col = era_tab$color_pspill, 
             legend = paste0("Era ",era_tab$era,": ",era_yrs_text), pch = 19)
      
    }
    if(return_lm_results==T){return(lm_results)}
  }
  
}



v_min_plots = function(plot_panel=NA){
  
  if(is.na(plot_panel) | plot_panel == 1){
    plot(dsb_tab$year, dsb_tab$min_fall_flow/10^6, 
         pch = 19, type = "o",
         main = "Min. 30-day fall flow vol. over time \n Observed and Predicted Min. Fall Flow",
         xlab = "Year", ylab = "V min (Mm3 / 30 days)")
    grid()
    points(dsb_tab$year, dsb_tab$AWI, pch = 18, type = "o", col="red")
    par(new=T)
    plot(dsb_tab$year, dsb_tab$min_fall_flow/10^6 * Mm3month_30day_to_cfs,
         axes=F, bty="n",xlab="",ylab="")
    axis(side=4, at=pretty(range(dsb_tab$min_fall_flow/10^6 * Mm3month_30day_to_cfs,
                                 na.rm=T)))
    mtext(text = "V min (cfs)", cex = .8,
          side = 4, line = 3.5)
    legend(x="topright",pch=c(19,18),col=c("black","red"),legend=c("Obs.","Pred."))
    
  }
  
  if(is.na(plot_panel) | plot_panel == 2){
    
    # plot residuals
    plot(dsb_tab$year, dsb_tab$AWI - dsb_tab$min_fall_flow/10^6,
         pch=19, type = "o", col="gray40", ylim = c(-12,5), 
         main = "Predicted minus observed min. fall flow volume \n (linear model residuals)",
         xlab = "Year", ylab = "Pred. minus obs. min. fall flow (Mm3 / 30 days)")
    grid()
    abline(h=0)
    
    # Add residuals for the other eras
    
    # Define one AWI for each era
    era1_sel = dsb_tab$year < 1977
    era2_sel = dsb_tab$year >= 1977 & dsb_tab$year < 2000
    era3_sel = dsb_tab$year >= 2000
    
    # Era 1
    tab_for_glm2a_E1 = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
                                            tab_for_glm[,pred1] * tab_for_glm[,pred2]) &
                                     tab_for_glm$era==1,]
    f2a = paste(c("min_fall_flow ~ ", paste(c(pred1, pred2), collapse = " + ")))
    glm_2a_E1 = glm(f2a, data = tab_for_glm2a_E1)
    pred_E1 = predict.glm(object = glm_2a_E1, newdata=dsb_tab)
    
    # Era 2
    tab_for_glm2a_E2 = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
                                            tab_for_glm[,pred1] * tab_for_glm[,pred2]) &
                                     tab_for_glm$era==2,]
    f2a = paste(c("min_fall_flow ~ ", paste(c(pred1, pred2), collapse = " + ")))
    glm_2a_E2 = glm(f2a, data = tab_for_glm2a_E2)
    pred_E2 = predict.glm(object = glm_2a_E2, newdata=dsb_tab)
    
    # Era 3
    tab_for_glm2a_E3 = tab_for_glm[!is.na(tab_for_glm$min_fall_flow * 
                                            tab_for_glm[,pred1] * tab_for_glm[,pred2]) &
                                     tab_for_glm$era==3,]
    f2a = paste(c("min_fall_flow ~ ", paste(c(pred1, pred2), collapse = " + ")))
    glm_2a_E3 = glm(f2a, data = tab_for_glm2a_E3)
    pred_E3 = predict.glm(object = glm_2a_E3, newdata=dsb_tab)
    
    # plot(dsb_tab$year, dsb_tab$min_30day_flow_jul_dec_cal_yr / 10^6, 
    #      type = "o", pch = 19)
    points(dsb_tab$year, pred_E1 - dsb_tab$min_30day_flow_jul_dec_cal_yr/10^6, #type = "o", 
           col = era_tab$color_minflow[1], pch = 18)
    points(dsb_tab$year, pred_E2 - dsb_tab$min_30day_flow_jul_dec_cal_yr/10^6, #type = "o",
           col = era_tab$color_minflow[2], pch = 18)
    points(dsb_tab$year, pred_E3 - dsb_tab$min_30day_flow_jul_dec_cal_yr/10^6, #type = "o",
           col = era_tab$color_minflow[3], pch = 18)
    
    legend(x = "bottomleft", pch = 18, col = c("black",era_tab$color_minflow),
           legend = c("Full Record",paste("Era",era_tab$era)), ncol = 2)
  }
  
  # # num for discussion
  # residuals = dsb_tab$min_fall_flow/10^6 - dsb_tab$AWI
  # resid_minus_outliers = residuals[residuals < 4]
  # resid_range_no_outliers = range(resid_minus_outliers,na.rm=T)
}



# Also, WYT over time
dwr_wyt_fig = function(){
  wyt = read.csv(file.path(local_data_dir,"DWR_SGMA_WYT_ScottR.csv"))
  
  wyt_cat = data.frame(descrip = c("Critical","Dry","Below Normal", "Above Normal", "Wet"),
                       descrip_abbrev = c("Crit.", "Dry", "BN", "AN", "Wet"),
                       color = c("firebrick","orangered","goldenrod","green3","dodgerblue"),
                       plot_val=1:5)
  # wyt$plot_val = wyt_cat$value[match(wyt$WYT, wyt_cat$descrip)]
  wyt = merge(wyt, wyt_cat, by.x="WYT", by.y="descrip")
  wyt=wyt[order(wyt$WY),]
  
  plot(wyt$WY, wyt$plot_val, yaxt = "n", col = NA,
       ylab = "DWR Water Year Type", xlab = "Water Year",
       main = "DWR Water Year Type for the Scott River watershed (HUC8 18010208)")
  grid()
  axis(side=2, at=wyt_cat$plot_val,labels=wyt_cat$descrip_abbrev, cex = .8)
  lines(wyt$WY, wyt$plot_val, col = "tan")
  points(wyt$WY, wyt$plot_val,col = wyt$color, pch=19)
  
}


p_spill_plots = function(tab_for_glm, pred1 = "USC00043182_oct_apr_mm_last_year",
                         pred2="SWJ_max_wc_mm", plot_panel = NA){
  
  if(is.na(plot_panel) | plot_panel==1){
    plot(dsb_tab$year, dsb_tab$cum_ppt_on_first_day_qspill, pch = 19, type = "o",
         main = "P spill over time, observed and predicted",
         xlab = "Year", ylab = "P spill (mm)", ylim = c(0,200))
    grid()
    points(dsb_tab$year, dsb_tab$p_spill_pred, pch = 18, type = "o", col="red")
    par(new=T)
    plot(dsb_tab$year, rep(NA, nrow(dsb_tab)),
         axes=F, bty="n",xlab="",ylab="", ylim = c(0,200) * mm_to_in)
    axis(side=4, at=pretty(c(0,200)*mm_to_in))
    mtext(text = "P spill (inches)", cex = .8,
          side = 4, line = 3.5)
    legend(x="topright",pch=c(19,18),col=c("black","red"),legend=c("Obs.","Pred."))
    
  }

  if(is.na(plot_panel) | plot_panel == 2){
    # plot residuals
    plot(dsb_tab$year, dsb_tab$p_spill_pred - 
           dsb_tab$cum_ppt_on_first_day_qspill,
         pch=19, type = "o", col="gray40", ylim = c(-120,100),
         main = "Predicted minus observed P spill \n (linear model residuals)",
         xlab = "Year", ylab = "Pred. minus obs. P spill (mm)")
    grid()
    abline(h=0)
    
    # Add residuals for the other eras
    
    # Define one P spill prediction for each era
    era1_sel = dsb_tab$year < 1977
    era2_sel = dsb_tab$year >= 1977 & dsb_tab$year < 2000
    era3_sel = dsb_tab$year >= 2000
    
    # Era 1
    tab_for_glm2a_E1 = tab_for_glm[!is.na(tab_for_glm$cum_ppt_on_first_day_qspill * 
                                            tab_for_glm[,pred1] *tab_for_glm[,pred2]  ) &
                                     tab_for_glm$era==1,]
    f2a = paste(c("cum_ppt_on_first_day_qspill ~  ", 
                  paste(c(pred1, pred2), collapse = " + ")))
    glm_2a_E1 = glm(f2a, data = tab_for_glm2a_E1)
    pred_E1 = predict.glm(object = glm_2a_E1, newdata=dsb_tab)
    
    # Era 2
    tab_for_glm2a_E2 = tab_for_glm[!is.na(tab_for_glm$cum_ppt_on_first_day_qspill * 
                                            tab_for_glm[,pred1] * tab_for_glm[,pred2]) &
                                     tab_for_glm$era==2,]
    f2a = paste(c("cum_ppt_on_first_day_qspill ~  ", 
                  paste(c(pred1, pred2), collapse = " + ")))
    glm_2a_E2 = glm(f2a, data = tab_for_glm2a_E2)
    pred_E2 = predict.glm(object = glm_2a_E2, newdata=dsb_tab)
    
    # Era 3
    tab_for_glm2a_E3 = tab_for_glm[!is.na(tab_for_glm$cum_ppt_on_first_day_qspill * 
                                            tab_for_glm[,pred1] * tab_for_glm[,pred2]) &
                                     tab_for_glm$era==3,]
    f2a = paste(c("cum_ppt_on_first_day_qspill ~  ", paste(c(pred1, pred2), collapse = " + ")))
    glm_2a_E3 = glm(f2a, data = tab_for_glm2a_E3)
    pred_E3 = predict.glm(object = glm_2a_E3, newdata=dsb_tab)
    
    # plot(dsb_tab$year, dsb_tab$min_30day_flow_jul_dec_cal_yr / 10^6, 
    #      type = "o", pch = 19)
    points(dsb_tab$year, pred_E1 - dsb_tab$cum_ppt_on_first_day_qspill, #type = "o", 
           col = era_tab$color_pspill[1], pch = 18)
    points(dsb_tab$year, pred_E2 - dsb_tab$cum_ppt_on_first_day_qspill, #type = "o",
           col = era_tab$color_pspill[2], pch = 18)
    points(dsb_tab$year, pred_E3 - dsb_tab$cum_ppt_on_first_day_qspill, #type = "o",
           col = era_tab$color_pspill[3], pch = 18)
    
    legend(x = "bottomleft", pch = 18, col = c("black",era_tab$color_pspill),
           legend = c("Full Record",paste("Era",era_tab$era)), ncol = 2)
    
  }

  # num for discussion
  # residuals = dsb_tab$cum_ppt_on_first_day_qspill/10^6 - dsb_tab$AWI
  # resid_minus_outliers = residuals[residuals < 4]
  # resid_range_no_outliers = range(resid_minus_outliers,na.rm=T)
}



# data exploration --------------------------------------------------------


precip_vs_fall_flows = function(xtype, wx_stns_plot = "all",n_colors = 4){
  if(wx_stns_plot =="all"){wx_stns_plot = wx_stns}
  if(xtype == "Total Precip"){
    # plot max water content vs Sep flows
    par(mfrow = c(2,2))
    for(i in 1:length(wx_stns_plot)){
      stn = wx_stns_plot[i]
      stn_long = noaa_stations$results.name[noaa_stations$station.id==stn]
      oct_apr_ppt = dsb_tab[,paste0(stn,"_oct_apr_mm")]
      flow_Mm3 = dsb_tab$sep_flow / 10^6
      plot(x= oct_apr_ppt,
           y = dsb_tab$sep_flow / 10^6,
           # xlim = c(0,85), ylim = c(0,20),
           xlab = "Oct-Apr Precip. (mm)", ylab = "Sep. Flow Vol (Mm3)",
           main = stn, sub = stn_long, pch = 19, col = rainbow(n_colors)[i])
      grid()
      if(sum(!is.na(oct_apr_ppt))>1 & 
         sum(!is.na(oct_apr_ppt) & oct_apr_ppt>0)>1){
        lm_stn = lm(flow_Mm3 ~ oct_apr_ppt)
        abline(lm_stn, lty = 2) # add line of best fit
      }
      text1 = paste0("Slope: ", round(lm_stn$coefficients[2], digits = 4))
      text2 = paste0("R square: ", round(summary(lm_stn)$r.square, digits = 2))
      legend(x= "topleft", col = NA,legend = c(text1, text2),
             bg=NA, bty="n")
    }
  }
  
  if(xtype == "Julian Day"){
    # plot max water content TIMING vs Sep flows
    par(mfrow = c(6,2))
    for(i in 1:12){#length(wx_stns)){
      stn = wx_stns[i]
      med_jday = dsb_tab[,paste0(stn,"_jday_of_median")]
      flow_Mm3 = dsb_tab$sep_flow / 10^6
      plot(x= med_jday,
           y = dsb_tab$sep_flow / 10^6,
           xlim = c(0,180), #ylim = c(0,20),
           xlab = "Mid. of Oct-Apr ppt (Days after Sep. 30)", ylab = "Sep. Flow Vol (Mm3)",
           main = stn, pch = 19, col = rainbow(length(wx_stns))[i])
      grid()
      if(sum(!is.na(med_jday))>1 & 
         sum(!is.na(med_jday) & med_jday>0)>1){
        lm_stn = lm(flow_Mm3 ~ med_jday)
        abline(lm_stn, lty = 2) # add line of best fit
      }
      text1 = paste0("Slope: ", round(lm_stn$coefficients[2], digits = 4))
      text2 = paste0("R square: ", round(summary(lm_stn)$r.square, digits = 2))
      legend(x= "topleft", col = NA,legend = c(text1, text2),
             bg=NA, bty="n")
    }
  }
  
}


add_dQ_dP_cols = function(fj3d, lag_days = 1, P, Q, 
                          dry_season_day1 = yday(as.Date("2001-09-01"))){
  # identify first date in each WY where dQ exceeds the threshold
  Q_lagged_forward = c(rep(NA, lag_days), Q[1:(length(Q)-lag_days)])
  P_lagged_forward = c(rep(NA, lag_days), P[1:(length(P)-lag_days)])
  
  # take out annual fenceposts - first x days of each year's dry season (which in this tabular arrangement have lagged values from the preceding year)
  dates_julian = yday(fj3d$Date)
  Q[dates_julian < (lag_days+dry_season_day1) & dates_julian >= dry_season_day1] = NA 
  P[dates_julian < (lag_days+dry_season_day1) & dates_julian >= dry_season_day1] = NA 
  
  dP = P - P_lagged_forward # real time cum. precip. record minus cum. ppt from x days ago
  dQ = Q - Q_lagged_forward # real time flow record minus flow from x days ago
  fj3d[,paste0("dP_lag",lag_days)] = dP
  fj3d[,paste0("dQ_lag",lag_days)] = dQ
  fj3d$dQ_dP = dQ/dP
  return(fj3d)
}


dQ_dP_difference_finder = function(q_thresh, EoDS_years){
  
  # initialize results table
  thresh_by_yr = expand.grid(Q_threshold_m3day = q_thresh, year = EoDS_years)
  results = thresh_by_yr
  results[,3:6]=NA
  colnames(results) = c(colnames(thresh_by_yr), "dQ_cum_pre", "dP_cum_pre",
                        "dQ_cum_post", "dP_cum_post")
  ## for each threshold:
  for(thresh in q_thresh){
    # initialize table of results for each threshold
    threshold_picker = results$Q_threshold_m3day==thresh
    ## for each water year:
    for(yr in EoDS_years){
      # find date of exceeding threshold
      # dQ_col_index = grepl(pattern = "dQ_lag",x = colnames(fj3d))
      Q_col_index = grepl(pattern = "Flow_m3day", x = colnames(fj3d))
      Q_exceeded_this_year = fj3d$Date>=as.Date(paste0(yr,"-09-30")) & #This is Sept 30, not Sept 1, because we need it to be at least a 30-day period starting sept 1
        fj3d$Date<=as.Date(paste0(yr+1,"-02-01")) & fj3d[, Q_col_index] > thresh
      
      # If the dQ threshold was exceeded in this end of dry season period, calculate dQs and dPs for 30 days on either side of the spill date
      if(sum(Q_exceeded_this_year, na.rm=T)>0){
        spill_date = min(fj3d$Date[Q_exceeded_this_year], na.rm=T)
        # calculate dQ_cum / dP_cum in the 30 days on either side of this date
        # dates_selector_30_days_prior = fj3d$Date < spill_date & fj3d$Date >= spill_date - 30
        spill_date_minus_30_days = spill_date - 29
        dQ_cum_prior = fj3d$cum_flow_m3[fj3d$Date == spill_date] -
          fj3d$cum_flow_m3[fj3d$Date==spill_date_minus_30_days] 
        dP_cum_prior = fj3d$cum_ppt[fj3d$Date == spill_date] -
          fj3d$cum_ppt[fj3d$Date==spill_date_minus_30_days] 
        
        # dates_selector_30_days_after = fj3d$Date >= spill_date & fj3d$Date < spill_date + 30
        spill_date_plus_30_days = spill_date + 29
        dQ_cum_post = fj3d$cum_flow_m3[fj3d$Date==spill_date_plus_30_days]  - fj3d$cum_flow_m3[fj3d$Date == spill_date]
        dP_cum_post = fj3d$cum_ppt[fj3d$Date==spill_date_plus_30_days] - 
          fj3d$cum_ppt[fj3d$Date == spill_date]
        
        yr_picker = results$year==yr
        sum(yr_picker & threshold_picker)
        results$dQ_cum_pre[yr_picker & threshold_picker] = dQ_cum_prior
        results$dQ_cum_post[yr_picker & threshold_picker] = dQ_cum_post
        results$dP_cum_pre[yr_picker & threshold_picker] = dP_cum_prior
        results$dP_cum_post[yr_picker & threshold_picker] = dP_cum_post
        
      }
    }
  }
  return(results)
}

gradient_areas_wl_vs_flow_day_of_and_lagged = function(){
  grad_months = 4:6
  # grad_low_flow = grad[grad$fj_flow_up_date<threshold_flow,]
  grad_spring = grad[month(grad$date_up) %in% grad_months,]
  
  
  for(area in grad_colors$area){
    grad_area = grad[grad$grad_areas==area,]
    
    grad_plot = grad_area
    
    par(mfrow = c(2,1))
    plot(grad_plot$elev_down, grad_plot$fj_flow_up_date_cfs,
         col = grad_plot$plot_color, pch = sym, log = "y",
         main = "Near-river wl elev obs vs FJ Flow, same day")
    grid()
    legend(x="topleft",pch=sym, legend=grad_colors$area, col = grad_colors$color)
    
    grad_plot = grad_area[month(grad_area$date_up) %in% grad_months,]
    plot(grad_plot$elev_down, grad_plot$fj_flow_cfs_lagged,
         col = grad_plot$plot_color, pch = sym, log = "y",
         main = "Spring near-river wl elev obs vs FJ Flow, 5 months later")
    grid()
    legend(x="topleft",pch=sym, legend=grad_colors$area, col = grad_colors$color)
  }
}



tot_dep_table_and_fig = function(nprc_flow, bau_flow, plot_type = "annual"){
  flow_diff = data.frame(Date = nprc_flow$Date,
                         Water_Year = wtr_yr(nprc_flow$Date),
                         nprc_minus_bau_m3d = nprc_flow$fj_flow_m3d - bau_flow$FJ_Flow_m3d,
                         nprc_minus_bau_cfs = nprc_flow$Flow - bau_flow$Flow)
  # plot(nprc_flow$Date, nprc_flow$Flow, type = "l", col = nprc_col)
  # lines(bau_flow$Date, bau_flow$Flow, col = bau_col)
  # eliminate 
  flow_diff_wy = aggregate(flow_diff$nprc_minus_bau_m3d, by = list(flow_diff$Water_Year), FUN = sum)
  sep_nov_selector = month(flow_diff$Date) %in% 9:11
  flow_diff_sep_nov = aggregate(flow_diff$nprc_minus_bau_m3d[sep_nov_selector], 
                                by = list(flow_diff$Water_Year[sep_nov_selector]), FUN = sum)
  
  
  wy_types = fj_wy_type_tab(fjd = fj_flow)#, show_pointplot = T)
  
  wy_types$tot_dep_sep_nov = flow_diff_sep_nov$x[match(wy_types$wy, flow_diff_sep_nov$Group.1)]
  wy_types$tot_dep_wy = flow_diff_wy$x[match(wy_types$wy, flow_diff_wy$Group.1)]
  
  wy_types$type = factor(x = wy_types$type, levels = c("Dry", "Below Avg", "Above Avg", "Wet"))
  
  # plot(x = wy_types$wy, y = wy_types$tot_dep_wy, col = wy_types$color, pch = 19)
  
  if(plot_type == "boxplot"){
    boxplot((tot_dep_sep_nov/10^6)~type, data = wy_types, col = year_type_colors$color,
            ylab = "Tot. Dep (sum of [NPRC - BAU], Sep-Nov '91-'18), million m3", 
            xlab = "Water Year Type (quartiles, WY 1942-2020)",
            cex.axis = 1.1, cex.lab = 1.2,
            ylim = c(0,13))
  }
  if(plot_type == "annual"){
    plot(flow_diff_sep_nov$Group.1, flow_diff_sep_nov$x / 10^6,
         type = "o", pch = 19,
         ylab = "Tot. Dep (sum of [NPRC - BAU], Sep-Nov '91-'18), million m3", 
         xlab = "Water Year")
    grid()
  }
  colnames(flow_diff_sep_nov)=c("water_year","tot_dep_sep_nov")
  return(flow_diff_sep_nov)
}


dep_reversed_table_and_fig = function(pma_flow, bau_flow, plot_type = "none"){
  flow_diff = data.frame(Date = pma_flow$Date,
                         Water_Year = wtr_yr(pma_flow$Date),
                         pma_minus_bau_m3d = pma_flow$FJ_Flow_m3d - bau_flow$FJ_Flow_m3d,
                         pma_minus_bau_cfs = pma_flow$Flow - bau_flow$Flow)
  # plot(nprc_flow$Date, nprc_flow$Flow, type = "l", col = nprc_col)
  # lines(bau_flow$Date, bau_flow$Flow, col = bau_col)
  # eliminate 
  flow_diff_wy = aggregate(flow_diff$pma_minus_bau_m3d, by = list(flow_diff$Water_Year), FUN = sum)
  sep_nov_selector = month(flow_diff$Date) %in% 9:11
  flow_diff_sep_nov = aggregate(flow_diff$pma_minus_bau_m3d[sep_nov_selector], 
                                by = list(flow_diff$Water_Year[sep_nov_selector]), FUN = sum)
  
  
  wy_types = fj_wy_type_tab(fjd = fj_flow)#, show_pointplot = T)
  
  wy_types$dep_rev_sep_nov = flow_diff_sep_nov$x[match(wy_types$wy, flow_diff_sep_nov$Group.1)]
  wy_types$dep_rev_wy = flow_diff_wy$x[match(wy_types$wy, flow_diff_wy$Group.1)]
  
  wy_types$type = factor(x = wy_types$type, levels = c("Dry", "Below Avg", "Above Avg", "Wet"))
  
  # plot(x = wy_types$wy, y = wy_types$tot_dep_wy, col = wy_types$color, pch = 19)
  
  if(plot_type =="boxplot"){
    boxplot((dep_rev_sep_nov/10^6)~type, data = wy_types, 
            col = year_type_colors$color, alpha = .5,
            ylab = "Dep. Rev. (sum of [PMA - BAU], Sep-Nov '91-'18), million m3", 
            xlab = "Water Year Type (quartiles, WY 1942-2020)",
            cex.axis = 1.1, cex.lab = 1.2, ylim = c(0,13))
  }
  
  colnames(flow_diff_sep_nov)=c("water_year","tot_dep_rev_sep_nov")
  return(flow_diff_sep_nov)
}

map_gw_flow_slopes = function(gw_flow){
  # map the slopes
  # Set up for plotting corr. coeff
  # slope_breaks = seq(from=min(wells_sp_cor$sep_slope,na.rm=T), 
  #                    to = max(wells_sp_cor$sep_slope,na.rm=T), length.out=7)
  
  slope_breaks = c(-60,-40,-20,0,20,40,60)
  # cor_breaks = seq(from=-1, to = 1, by = .25)
  n_classes = length(slope_breaks)-1
  my_palette_name = "Blue-Red"# "Oslo"
  my_palette = rev(diverging_hcl(n_classes, palette = my_palette_name))
  
  colname_window = "sep_slope"
  
  classification_vector = cut(wells_sp_cor@data[,colname_window],
                              include.lowest=T,
                              breaks = slope_breaks)
  color_vector = my_palette[classification_vector]
  
  
  # Pick thresholds to ID high-value predictive wells
  highlight_wells = gw_flow$well_code[gw_flow$sep_cor>0.5 & 
                                        gw_flow$oct_cor>0.5 & 
                                        #gw_flow$sep_slope > 0 & 
                                        # ^ don't need this! slope and cor are related
                                        !is.na(gw_flow$sep_cor)]
  
  plot(wells_sp[wells_sp$well_code %in% highlight_wells,], add=T, pch = 5, 
       bg=NA, lwd=2, cex = 1.2, col = "red")
  
  legend_text = paste(round(slope_breaks[1:(n_classes)], 1), 
                      round(slope_breaks[2:(n_classes+1)], 1), sep = " to ")
  legend(x="bottomleft", #pt.bg = my_palette, pch=21,
         pt.cex=1.5, col = my_palette, pch=19,
         legend = legend_text)
  
}

show_gw_fall_flows_scatter = function(dsb_tab, highlight_wells){
  wl_cols = grep(pattern = "aprWL", x= colnames(dsb_tab))
  wl_colors = rainbow(n=length(highlight_wells))
  
  # par(mfrow=c(4,2))
  par(mfrow = c(2,1))
  for(i in 1:length(wl_cols)){
    wl_col = wl_cols[i]
    well_id = unlist(strsplit(colnames(dsb_tab)[wl_col], split = "aprWL_"))[2]
    wls = dsb_tab[,wl_col]
    flows = dsb_tab$min_fall_flow/10^6
    plot(x = wls, y = flows,
         pch=19, col = wl_colors[i],
         main = well_id, xlab = "April GW Elev (ft amsl)",
         ylab = "Sept. flow (Mm3)")
    
    lm_well = lm(flows~wls)
    abline(lm_well, lty = 2)
    
    text1 = paste0("Slope: ", round(lm_well$coefficients[2], digits = 2))
    text2 = paste0("R square: ", round(summary(lm_well)$r.square, digits = 2))
    legend(x= "topleft", col = NA,legend = c(text1, text2),
           bg=NA, bty="n")
    
  }
  
}

stream_aq_flux_hist = function(summary_tab){
  # discharge vs leakage histograms
  par(mfrow = c(3,1))
  hist(summary_tab$tot_flow_to_stm, breaks = seq(-.4,2.5,0.1), ylim = c(0,150))
  abline(v=0)
  hist(summary_tab$tot_flow_to_aq, breaks = seq(-.4,2.5,0.1), ylim = c(0,150))
  abline(v=0)
  hist(summary_tab$net_flow_to_stm, breaks = seq(-.4,2.5,0.1), ylim = c(0,150))
  abline(v=0)
  
}

gradients_vs_flow_plot = function(Q_spill=120){
  grad_symbol = 18
  grad_colors = data.frame(area = unique(grad$grad_areas),
                           name = c("Tailings","French C", "Etna Cr",
                                    "b/t Etna and Gvw", "Greenview",
                                    "Hamlin", "Moffet", "Quartz V"),
                           color = colorblind_pal()(8))
  grad$plot_color = grad_colors$color[match(grad$grad_areas, grad_colors$area)]
  
  
  
  threshold_flow = Q_spill
  # grad_low_flow = grad[grad$fj_flow_up_date<threshold_flow,]
  # grad_fre = grad[grad$grad_areas=="fre" & grad$fj_flow_up_date < threshold_flow,]
  
  grad_plot = grad#_fre
  
  par(mfrow = c(2,1))
  plot(grad_plot$gradient, grad_plot$fj_flow_down_date,
       col = grad$plot_color, pch = grad_symbol, log = "y",
       main = "Observed gradients vs FJ Flow")
  grid()
  legend(x="topleft",pch=grad_symbol, legend=grad_colors$area, col = grad_colors$color)
  
  plot(grad_plot$gradient_sim, grad_plot$fj_flow_sim_m3_month,
       col = grad$plot_color, pch = grad_symbol, log = "y",
       main = "Sim gradients vs Sim FJ Flow")
  # ,
  #      xlab = paste("Obs: Gradients (m/m) from wl obs. a max of",wl_obs_days,"apart"),
  #      ylab = "Sim: Gradients (m/m) from SVIHM results, matching obs. loc. and SP")
  
  # abline(a=0,b=1)
  grid()
}


add_fall_rains_to_fall_flows_tab = function(dsb_tab,
                                            fall_rain_start_day = "09-01",
                                            fall_rain_end_day = "03-01",
                                            max_rain = 1500, # mm, right?
                                            show_cum_rainfall_plot = T,
                                            print_NAs = F,
                                            precip_thresholds = c(25, 50, 75, #mm, ~ 1:6 inches
                                                                  100, 125, 150) ){
  
  # make copy of fall flow tab
  fft = dsb_tab
  for(j in 1:length(keep_wx)){
    stn = keep_wx[j]
    # initialize columns in fall flow tab to receive the rainfall exceedance timing
    fft[,paste0(stn,"_wy_day_exceed_",precip_thresholds,"mm")] = NA
    
    if(show_cum_rainfall_plot==T){ #initialize plot
      n_days_in_fall_leap = as.numeric(as.Date(paste0("2004-",fall_rain_end_day)) -
                                         as.Date(paste0("2003-",fall_rain_start_day))) + 1
      n_days_in_fall = as.numeric(as.Date(paste0("2002-",fall_rain_end_day)) -
                                    as.Date(paste0("2001-",fall_rain_start_day))) +1
      stn_yr_range = range(year(noaa$DATE[noaa$STATION==stn]))
      plot(x=1:n_days_in_fall_leap, 
           y=seq(from=0,to=max_rain, length.out = n_days_in_fall_leap),
           xlab = "Days since Aug. 31", ylab = "Cumulative Precip. (mm)",
           pch=NA, col = NA, 
           main = paste0(noaa_stations$results.name[noaa_stations$station.id==stn],
                         " (",stn,")"), 
           sub = paste(stn_yr_range, collapse = "-"))
    }
    for(i in 1:length(dsb_tab)){
      year_i=dsb_tab$year[i]
      fall_date1 = as.Date(paste0(year_i,"-",fall_rain_start_day)) 
      #^ choose Sept. 1 for salmon consistency. consider Oct 1 at some point
      fall_date2 = as.Date(paste0(year_i+1,"-",fall_rain_end_day))
      fall_rainfall_wy = noaa[noaa$STATION==stn & 
                                noaa$DATE >= fall_date1 &
                                noaa$DATE <= fall_date2,]
      if(print_NAs){print(paste("Num NAs,",year_i, ":",sum(is.na(fall_rainfall_wy$PRCP))))}
      fall_rainfall_wy$PRCP[is.na(fall_rainfall_wy$PRCP)] = 0 # clean up for cumsum
      fall_cumppt = cumsum(fall_rainfall_wy$PRCP)
      # calculate exceedances
      for(k in precip_thresholds){
        if(!is.infinite(min(which(fall_cumppt > k)))){ # if there's an exceedance date
          fft[i,paste0(stn,"_wy_day_exceed_",k,"mm")] = min(which(fall_cumppt > k))
        }
      }
      if(show_cum_rainfall_plot==T){ 
        if(nrow(fall_rainfall_wy)>=n_days_in_fall){ # add line to plot if has a full rainy season
          if(nrow(fall_rainfall_wy)==n_days_in_fall){
            lines(x=1:n_days_in_fall, y = fall_cumppt, col = rgb(0.1,0.1,.8,.3))
          } else { # if in a leap year, make a new x-vector with 1 extra day
            lines(x=1:n_days_in_fall_leap, y = fall_cumppt, col = rgb(0.1,0.1,.8,.3))
          }
        }
      }
    }
    
  }
  return(fft)
  
}



# one-time analyses -------------------------------------------------------

linear_model_forward_selection = function(){
  
  ### subfunctions
  show_lm_summary_from_table = function(pred_tab, data_tab, y_name = "min_fall_flows"){
    # assumes each predictor is in a column with an "x1", "x2" name, etc
    x_cols_selector = grepl(pattern = "x", x = colnames(pred_tab))
    num_xs = sum(x_cols_selector)
    
    f1 = paste(y_name,
               paste(pred_tab[1,x_cols_selector], collapse = " + "), sep = " ~ ")
    lm1 = lm(data = data_tab, formula = f1)
    print(summary(lm1))
    
    # str(lm1)
  }
  
  make_table_of_all_lms = function(data_tab, y_name = "coho_smolt_per_fem", num_xs = 1){
    
    # Initialize output tab as a combination of 
    non_pred_columns = c("brood_year","smolt_year", "chinook_spawner_abundance",
                         "coho_spawner_abundance", "coho_smolt_per_fem",
                         "coho_smolt_abun_est","percent_coho_smolt_survival")
    data_tab_pred = data_tab[,!(colnames(data_tab) %in% non_pred_columns)]
    cdtb = colnames(data_tab_pred)
    if(num_xs==1){model_combos = cdtb}
    if(num_xs==2){model_combos = expand.grid(cdtb, cdtb)}
    if(num_xs==3){model_combos = expand.grid(cdtb, cdtb, cdtb)}
    if(num_xs==4){model_combos = expand.grid(cdtb, cdtb, cdtb, cdtb)}
    if(num_xs==5){model_combos = expand.grid(cdtb, cdtb, cdtb, cdtb, cdtb)}
    if(num_xs==6){model_combos = expand.grid(cdtb, cdtb, cdtb, cdtb, cdtb, cdtb)}
    
    output_tab = as.data.frame(as.matrix(model_combos))
    pred_colnames = paste0("x", 1:num_xs)
    colnames(output_tab) = pred_colnames
    # eliminate models with repeat predictors. ugh. hard to do this without knowing the # of predictors
    # dup_rows = (output_tab$x1==output_tab$x2) | (output_tab$x2==output_tab$x3) | (output_tab$x1==output_tab$x3)
    # output_tab = output_tab[!dup_rows,]
    
    # add model 
    output_tab$Fstat = NA; output_tab$pval = NA
    output_tab$Rsquare = NA; output_tab$adjRsquare = NA
    output_tab$AICc = NA
    
    for(i in 1:nrow(output_tab)){
      predictors = as.character(output_tab[i, 1:num_xs])
      
      if(length(unique(predictors)) == num_xs){ # weed out combos with redundant factors (again)
        f1 = paste(y_name, 
                   paste(predictors, collapse = " + "), sep = " ~ ")
        lm1 = lm(data = data_tab, formula = f1)
        coeffs = summary(lm1)$coefficients
        
        if(sum(is.na(coeffs))<1){  # Don't populate the output tab if there's insufficient pairwise complete data
          #assign output tab vars
          output_tab$Fstat[i] = summary(lm1)$fstatistic
          output_tab$pval[i] =  coeffs[1,ncol(coeffs)]
          output_tab$Rsquare[i] = summary(lm1)$r.squared
          output_tab$adjRsquare[i] = summary(lm1)$adj.r.squared
          output_tab$AICc[i] = AICc(lm1)
        }
      }
    }
    
    return(output_tab)
  }
  
  
  ### 0. Prepare reduced table of metrics
  #reduced metrics tab for the 4-predictor models
  
  # reduced_pred = c("BY_FA_Mag", "BY_recon_10", "BY_recon_100", "BY_tot_flow_sepdec",
  #                  "RY_discon_10", "RY_discon_100", "RY_DS_Mag_50", "RY_DS_Mag_90", "RY_FA_Mag", 
  #                  "RY_recon_10", "RY_recon_100", "RY_SP_ROC", "RY_tot_flow", "RY_Wet_BFL_Dur", "RY_Wet_Tim", 
  #                  "SY_discon_100", "SY_SP_ROC", "SY_tot_flow_janjul", "SY_Wet_BFL_Dur",
  #                  "SY_Wet_Tim", "tot_flow_CFLP")
  reduced_pred = c("BY_FA_Mag", "BY_recon_10", #"BY_recon_15", "BY_recon_20", 
                   "BY_recon_100", "BY_tot_flow_sepdec",
                   "RY_discon_10", "RY_discon_15", "RY_discon_20", "RY_discon_100", 
                   "RY_DS_Mag_50", "RY_DS_Mag_90", "RY_FA_Mag", 
                   "RY_recon_10", "RY_recon_15", "RY_recon_20", "RY_recon_100", 
                   "RY_SP_ROC", "RY_tot_flow", "RY_Wet_BFL_Dur", "RY_Wet_Tim", 
                   "SY_discon_100", "SY_SP_ROC", "SY_tot_flow_janjul", "SY_Wet_BFL_Dur",
                   "SY_Wet_Tim", "tot_flow_CFLP")
  keep_cols = c(colnames(metrics_tab)[1:7],reduced_pred)
  metrics_tab_redu = metrics_tab[,keep_cols]
  
  ### 1. Selection of best 1 predictor model
  pred1 = make_table_of_all_lms(data_tab = metrics_tab_redu, y_name = "coho_smolt_per_fem", num_xs = 1)
  
  # Which are the best? Want a high Rsquare and a low pval
  plot(pred1$pval, pred1$Rsquare) # choose a quadrant 
  hist(pred1$AICc, xlim = c(0,140))
  AICc_threshold = quantile(pred1$AICc, 0.5)
  best_1 = pred1[pred1$Rsquare>.3 & pred1$pval < .1 & pred1$AICc < AICc_threshold,]
  best_1
  # Best is BY_recon_10, based on pval and Rsquare, but AICc is high (107). 
  # RY_Wet_Tim is similar. (and BY_recon_15)
  # Alternate best is RY_FA_Mag. Much better AICc of 73. 
  
  best1_sub = best_1[best_1$x1=="BY_recon_10",]
  show_lm_summary_from_table(pred_tab = best1_sub, data_tab = metrics_tab_redu)
  
  
  
  ### 2. Selection of best 2 predictor model
  pred2 = make_table_of_all_lms(data_tab = metrics_tab_redu, y_name = "coho_smolt_per_fem", num_xs = 2)
  pred2 = pred2[!is.na(pred2$pval),] # get rid of NA rows
  # # get rid of redundant combos
  # pred2_alpha_order = pred2$x1 < pred2$x2 # find the ones in alphabetical order
  # pred2_pred_combo = apply(X= as.character(pred2[,]), MARGIN = 1, FUN = paste)
  # pred2_pred_combo[!pred2_alpha_order] = paste(pred2$x2, pred2$x1, collapse = "_")
  # pred2 = duplicated(pred2_pred_combo)
  
  pred2$Fstat = round(pred2$Fstat, 1); pred2$pval = round(pred2$pval, 3)
  pred2$Rsquare = round(pred2$Rsquare, 3); pred2$adjRsquare = round(pred2$adjRsquare, 3)
  pred2$AICc = round(pred2$AICc, 1)
  # Which are best?
  # pred2_nofamag = pred2[!(grepl(pattern = "FA_Mag", x = pred2$x1) | 
  #                           grepl(pattern = "FA_Mag", x = pred2$x2)),]
  plot(pred2$pval, pred2$Rsquare)
  hist(pred2$AICc, xlim = c(0,140))
  AICc_threshold = quantile(pred2$AICc, 0.5)
  best_2 = pred2[pred2$Rsquare > .6 & pred2$pval < 0.2 & pred2$AICc<AICc_threshold,]
  dim(best_2)
  # which ones show up the most in the best models?
  x1_freq = aggregate(best_2$x1, by = list(best_2$x1), FUN = length)
  x1_freq[order(x1_freq$x, decreasing = T),]
  
  # Definition of best: R2 > 0.7, pval <0.1, and AICc < 112 (50th %ile). 12 models.
  # In 3 best models: BY_recon_10
  # In 2 best models: BY_FA_Mag, RY_Wet_Tim, BY_recon_100, RY_DS_Mag_90s
  
  best_2_sub = best_2[best_2$x1=="BY_recon_10" & 
                        best_2$x2 %in% c("BY_recon_100",
                                         "RY_Wet_Tim"),]
  show_lm_summary_from_table(pred_tab = best_2_sub[1,], data_tab = metrics_tab_redu)
  show_lm_summary_from_table(pred_tab = best_2_sub[2,], data_tab = metrics_tab_redu)
  show_lm_summary_from_table(pred_tab = pred2[pred2$x1=="BY_recon_10" & pred2$x2=="SY_SP_ROC",], data_tab = metrics_tab_redu)
  
  best_2_sub = best_2[best_2$x1=="BY_recon_10",]
  
  ### 3. Selection of best 3-predictor model
  
  pred3 = make_table_of_all_lms(data_tab = metrics_tab_redu, y_name = "coho_smolt_per_fem", num_xs = 3)
  
  pred3 = pred3[!is.na(pred3$pval),] # get rid of NA rows
  pred3$Fstat = round(pred3$Fstat, 1); pred3$pval = round(pred3$pval, 3)
  pred3$Rsquare = round(pred3$Rsquare, 3); pred3$adjRsquare = round(pred3$adjRsquare, 3)
  pred3$AICc = round(pred3$AICc, 1)
  # Which are best?
  plot(pred3$pval, pred3$Rsquare)
  # plot(pred3$pval, pred3$AICc)   # saturation relat
  # plot(pred3$Rsquare, pred3$AICc) # negative saturation relat. 
  hist(pred3$AICc, xlim = c(0,140))
  AICc_threshold = quantile(pred3$AICc, 0.3)
  best_3 = pred3[pred3$Rsquare > .7 & pred3$pval < 0.1 & pred3$AICc<AICc_threshold,]
  dim(best_3)
  # which ones show up the most in the best models?
  x1_freq = aggregate(best_3$x1, by = list(best_3$x1), FUN = length)
  x1_freq[order(x1_freq$x, decreasing = T),]
  # Best definition, 3 predictors round: R2 > 0.7, pval < 0.1, AICc < 103.7 (30th %ile). 228 models of 48,714.
  
  # # FA_Mags are involved in a lot of the best models but have 5 missing values, so they reduce the sample size a lot. 
  famags = c("BY_FA_Mag", "RY_FA_Mag", "RY_DS_Mag_90","RY_DS_Mag_50")
  best_3_nofamag = best_3[!(best_3$x1 %in% famags | best_3$x2 %in% famags | best_3$x3 %in% famags),]
  # View(best_3_nofamag)
  # x1_freq_nof = aggregate(best_3_nofamag$x1, by = list(best_3_nofamag$x1), FUN = length)
  # x1_freq_nof[order(x1_freq_nof$x, decreasing = T),]
  
  #In the best models excluding the suspicious predictors,
  # 26 models: BY_recon_10
  # 18 models: BY_recon_100
  # 10 models: RY_Wet_Tim
  # 4 models: RY_recon_10, RY_Wet_BFL_Dur, and SY_Wet_BFL_Dur.
  
  best3_suba = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="BY_recon_100" & !(best_3$x3 %in% famags),]
  best3_subb = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="RY_Wet_Tim" & !(best_3$x3 %in% famags),]
  # # View(best3_subb)
  
  
  ### 4. Selection of best 4-predictor model
  
  pred4 = make_table_of_all_lms(data_tab = metrics_tab_redu, y_name = "coho_smolt_per_fem", num_xs = 4)
  
  pred4 = pred4[!is.na(pred4$pval),] # get rid of NA rows
  pred4$Fstat = round(pred4$Fstat, 1); pred4$pval = round(pred4$pval, 3)
  pred4$Rsquare = round(pred4$Rsquare, 3); pred4$adjRsquare = round(pred4$adjRsquare, 3)
  pred4$AICc = round(pred4$AICc, 1)
  # Which are best?
  plot(pred4$pval, pred4$Rsquare)
  # plot(pred4$pval, pred4$AICc)   # saturation relat
  # plot(pred4$Rsquare, pred4$AICc) # negative saturation relat. 
  AICc_threshold = quantile(pred4$AICc, 0.5)
  best_4 = pred4[pred4$Rsquare > .7 & pred4$pval < 0.1 & pred4$AICc<AICc_threshold,]
  dim(best_4)
  # which ones show up the most in the best models?
  x1_freq = aggregate(best_4$x1, by = list(best_4$x1), FUN = length)
  x1_freq[order(x1_freq$x, decreasing = T),]
  
  #take out suspicious predictors
  famags = c("BY_FA_Mag", "RY_FA_Mag", "RY_DS_Mag_90","RY_DS_Mag_50")
  best_4_nofamag = best_4[!(best_4$x1 %in% famags | best_4$x2 %in% famags |
                              best_4$x3 %in% famags | best_4$x4 %in% famags),]
  x1_freq_nof = aggregate(best_4_nofamag$x1, by = list(best_4_nofamag$x1), FUN = length)
  x1_freq_nof[order(x1_freq_nof$x, decreasing = T),]
  
  #In the best models excluding the suspicious predictors,
  # 540 models: BY_recon_10
  # 324 models: BY_recon_100
  # 252 models: RY_Wet_Tim
  
  # 138: SY_tot_flow_janjul
  # 132: SY_Wet_BFL_Dur
  # 126: RY_recon_10
  # 120: RY_SP_ROC
  # 102-114 models: BY_tot_flow_sepdec, RY_recon_100, tot_flow_CFLP, RY_tot_flow
  
  
  # # Best model subsetting
  best4_sub1 = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                       pred4$x3=="RY_Wet_Tim" & !(pred4$x4 %in% famags),]
  best4_sub2 = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                       pred4$x3=="SY_tot_flow_janjul" & !(pred4$x4 %in% famags),]
  best4_sub3 = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                       pred4$x3=="SY_Wet_BFL_Dur" & !(pred4$x4 %in% famags),]
  best4_sub4 = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                       pred4$x3=="RY_recon_10" & !(pred4$x4 %in% famags),]
  best4_sub5 = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                       pred4$x3=="RY_SP_ROC" & !(pred4$x4 %in% famags),]
  
  View(best4_sub3)
  # dim(best4_sub)
  
  # # what about these FAMags. Damn, they all have an FA_Mag.
  # famags = c("BY_FA_Mag", "RY_FA_Mag")
  # best_4_nofamag = best_3[!(best_4$x1 %in% famags | best_4$x2 %in% famags | 
  #                             best_4$x3 %in% famags | best_4$x4 %in% famags),]
  
  
  
  ### 5. Best Models and lm Summaries - 1/28/2022
  
  #1 pred comp
  best1 = pred1[pred1$x1=="BY_recon_10",]
  show_lm_summary_from_table(pred_tab = best1, data_tab = metrics_tab_redu)
  # 2 pred comp
  best2_a = best_2[best_2$x1=="BY_recon_10" & best_2$x2=="BY_recon_100",]
  show_lm_summary_from_table(pred_tab = best2_a, data_tab = metrics_tab_redu)
  best2_b = best_2[best_2$x1=="BY_recon_10" & best_2$x2=="RY_Wet_Tim",]
  show_lm_summary_from_table(pred_tab = best2_b, data_tab = metrics_tab_redu)
  # 3 pred comp
  best3_a = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="BY_recon_100" & best_3$x3=="RY_Wet_BFL_Dur",]
  show_lm_summary_from_table(pred_tab = best3_a, data_tab = metrics_tab_redu)
  best3_b = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="RY_Wet_Tim" & best_3$x3=="SY_SP_ROC" ,]
  show_lm_summary_from_table(pred_tab = best3_b, data_tab = metrics_tab_redu)
  # 4 pred comp
  best4_a = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                    pred4$x3=="RY_Wet_BFL_Dur"& pred4$x4=="RY_SP_ROC",]
  show_lm_summary_from_table(pred_tab = best4_a, data_tab = metrics_tab_redu)
  best4_b = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="RY_Wet_Tim" &
                    pred4$x3=="SY_SP_ROC"& pred4$x4=="RY_discon_10",]
  show_lm_summary_from_table(pred_tab = best4_b, data_tab = metrics_tab_redu)
  
  
  ### 5. Best Models and lm Summaries - 1/31/2022
  
  #1 pred comp
  best1 = pred1[pred1$x1=="BY_recon_10",]
  show_lm_summary_from_table(pred_tab = best1, data_tab = metrics_tab_redu)
  # 2 pred comp
  best2_a = best_2[best_2$x1=="BY_recon_10" & best_2$x2=="BY_recon_100",]
  show_lm_summary_from_table(pred_tab = best2_a, data_tab = metrics_tab_redu)
  best2_b = best_2[best_2$x1=="BY_recon_10" & best_2$x2=="RY_Wet_Tim",]
  show_lm_summary_from_table(pred_tab = best2_b, data_tab = metrics_tab_redu)
  # 3 pred comp
  best3_a = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="BY_recon_100" & best_3$x3=="RY_Wet_BFL_Dur",]
  show_lm_summary_from_table(pred_tab = best3_a, data_tab = metrics_tab_redu)
  best3_b = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="BY_recon_100" & best_3$x3=="RY_Wet_Tim" ,]
  show_lm_summary_from_table(pred_tab = best3_b, data_tab = metrics_tab_redu)
  # 4 pred comp
  best4_a = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                    pred4$x3=="RY_Wet_BFL_Dur"& pred4$x4=="RY_SP_ROC",]
  show_lm_summary_from_table(pred_tab = best4_a, data_tab = metrics_tab_redu)
  best4_b = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                    pred4$x3=="RY_Wet_Tim"& pred4$x4=="SY_SP_ROC",]
  show_lm_summary_from_table(pred_tab = best4_b, data_tab = metrics_tab_redu)
  
  ### 5. Best Models and lm Summaries - 2/06/2022
  
  #1 pred comp
  best1 = pred1[pred1$x1=="BY_recon_10",]
  show_lm_summary_from_table(pred_tab = best1, data_tab = metrics_tab_redu)
  # 2 pred comp
  best2_a = best_2[best_2$x1=="BY_recon_10" & best_2$x2=="BY_recon_100",]
  show_lm_summary_from_table(pred_tab = best2_a, data_tab = metrics_tab_redu)
  best2_b = best_2[best_2$x1=="BY_recon_10" & best_2$x2=="RY_Wet_Tim",]
  show_lm_summary_from_table(pred_tab = best2_b, data_tab = metrics_tab_redu)
  # 3 pred comp
  best3_a = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="BY_recon_100" & best_3$x3=="RY_Wet_BFL_Dur",]
  show_lm_summary_from_table(pred_tab = best3_a, data_tab = metrics_tab_redu)
  best3_b = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="RY_Wet_Tim" & best_3$x3=="SY_SP_ROC" ,]
  show_lm_summary_from_table(pred_tab = best3_b, data_tab = metrics_tab_redu)
  # 4 pred comp
  best4_a = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                    pred4$x3=="RY_Wet_Tim"& pred4$x4=="RY_Wet_BFL_Dur",]
  show_lm_summary_from_table(pred_tab = best4_a, data_tab = metrics_tab_redu)
  best4_b = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                    pred4$x3=="RY_SP_ROC"& pred4$x4=="RY_Wet_BFL_Dur",]
  show_lm_summary_from_table(pred_tab = best4_b, data_tab = metrics_tab_redu)
  
  
  ### 6. AICc comparisons
  par(mfrow = c(2,2))
  AICc_breaks = seq(from=-20, to = 150, by = 5)
  hist(pred1$AICc, xlim = c(-20,140), breaks = AICc_breaks, freq=F, ylim = c(0,.08),
       xlab = "AICc of linear models", main = "Histogram of AICc for 1-predictor models")
  grid()
  hist(pred2$AICc, xlim = c(-20,140), breaks = AICc_breaks, freq=F,ylim = c(0,.08),
       xlab = "AICc of linear models", main = "Histogram of AICc for 2-predictor models")
  grid()
  hist(pred3$AICc, xlim = c(-20,140), breaks = AICc_breaks, freq=F,ylim = c(0,.08),
       xlab = "AICc of linear models", main = "Histogram of AICc for 3-predictor models")
  grid()
  hist(pred4$AICc, xlim = c(-20,140), breaks = AICc_breaks, freq=F,ylim = c(0,.08),
       xlab = "AICc of linear models", main = "Histogram of AICc for 4-predictor models")
  grid()
  
  
  
}


predict_fall_vmin_pspill = function(num_days_missing = 7){
  # load and update data
  # first: load libraries from top of data retrieval script
  
  
  # From Data Retrieval script:
  # Update NOAA data and isolate FJRS record for relevant water year, Oct 1-April 30
  fjrs = get_noaa_data(station_list = "USC00043182")
  str(fjrs)
  
  wy = 2023 # set water year
  
  rain_bracket_dates = as.Date(c(paste0(wy-1,"-10-01"), paste0(wy, "-04-30")))
  # check that those bracket dates aren't in gaps in the record
  if(sum(fjrs$DATE==rain_bracket_dates[1])==0){ # If this date does not exist in the record
    # first check starting date
    last_date_with_data = max(fjrs$DATE[fjrs$DATE<rain_bracket_dates[1]]) # find the next date back with data
    if(as.numeric(diff(c(last_date_with_data, rain_bracket_dates[1])))<=num_days_missing){ # If it's a week off or less
      rain_bracket_dates[1]=last_date_with_data # reset the bracket date to be the last date with data
    }
    
    # next, check ending date
    next_date_with_data = min(fjrs$DATE[fjrs$DATE>rain_bracket_dates[2]]) # find the next date in time with data
    if(as.numeric(diff(c(next_date_with_data, rain_bracket_dates[2])))<=num_days_missing){ # If it's a week off or less
      rain_bracket_dates[2]=next_date_with_data # reset the bracket date to be the last date with data
    }
  }
  fjrs_wy = fjrs[fjrs$DATE>=rain_bracket_dates[1] &
                   fjrs$DATE<=rain_bracket_dates[2],]
  tail(fjrs_wy)
  # fjrs_wy
  # Finally, find cumulative precip in this wet season
  fjrs_cum_wy = sum(fjrs_wy$PRCP, na.rm=T)
  
  # Snow data: if cdec retrieve still isn't working, re-download the new snow records.
  # save in snow data folder.
  unique(snow_cdec$Station_code)
  swj = snow_cdec[snow_cdec$Station_code=="SWJ" ,]
  swj$Date=as.Date(swj$Date, format = "%d-%b-%Y")
  
  swj_max_mm_wy = max(swj$Water_Content_in[year(swj$Date)==wy]) * in_to_mm
  
  # Calculate Pspill and Vmin
  vmin_i = -1.33+.00525*fjrs_cum_wy + .00267*swj_max_mm_wy
  vmin_i_cfs = vmin_i * Mm3month_30day_to_cfs
  
  pspill_i = 123 - 0.111*fjrs_cum_wy - .0274*swj_max_mm_wy
  pspill_i_in = pspill_i * mm_to_in
}

