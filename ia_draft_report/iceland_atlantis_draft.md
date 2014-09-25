# Iceland Atlantis Report (DRAFT)
######Christopher David Desjardins
######24 September 2014

## Setting up the Modelling Domain

Atlantis requires a special spatial file format. This is termed a *bgm* file (box geometry file). The *bgm* file defines the geometry (i.e. bathmetry) used in the Atlantis model. The *bgm* file stores the spatial information in *x* and *y* terms rather than latitude and longitude. Information about the projection (used in the visualization of the spatial data and for determining the day length), number of boxes, number of dynamic faces, maximum depth, vertical mixing and horizontal transport scalars, whether a box is a boundary box (i.e. is it dynamic, meaning does it border the open ocean) and the spatial layout of the boxes and  the faces (e.g. area, location of vertices, depth of box) are specified in the *bgm* file. 

For the Icelandic Atlantis model, data provided by the Marine Research Institute (MRI) in Reykjavík contained in the *reitmapping.tsv* and the `R` `geo` package were used to develop the modelling domain. The subdivisions used are described in Anonymous (2004)[^7]. The subdivisions were exported from `R` to `QGIS` where they were modified to not overlap with Iceland, Greenland, and the Faroe Islands and boundary boxes were added around the subdivisions. The boundary boxes, generally, buffer the active model area by 1/2 degree longitude and 1/4 degree latitude. These boundary boxes were selected to be make the interpolation of the hydrography data easier. The modelling domain of the Iceland Atlantis model is shown below. 
![Smaller icon](figures/iceland_atlantis_L93.png). 

Comparing the figure with the Figure 9.7 in Anonymous (2004), the location and, generally, uniform width of the boundary box around the subdivisions is easily discernible. 

On the CSIRO wiki, there are various tools to help generate a *bgm* file. In particular the `bgmeriser.jar` tool was used after the shapefile was cleaned in `QGIS` and `GRASS`. The projection used was ISN93/Lambert 1993, which corresponds to the following `proj4` format:   
    
    +proj=lcc +lat_1=64.25 +lat_2=65.75
    +lat_0=65 +lon_0=-19 +x_0=500000 
    +y_0=500000 +ellps=GRS80
    +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 

 The maximum depth for each box in the modelling domain was calculated using data  provided in `geo::gbdypi`. The depths of the boxes ranged from 100m to 3800m. Iceland and the Faroe Islands, while included in the modelling domain, were assigned a depth of 0 in the model in order to avoid having islands in the model
 
 For the Icelandic Atlantis model, the water column was split up into at most six water column layers: 0 - 50 m, 50 - 150 m, 150 - 300 m, 300 - 600m, 600 - 1000m, 1000m+ with one sediment layer. The size of these layers was selected after consultation with researchers at MRI and these layers are similar to depth layers reported Atlantis models (Link, Fulton, & Gamble, 2010[^8]; Savina, Fulton, Condie, Forrest, Scandol, & Astles, 2008[^9]).


This is [an example][id] reference-style link.
[id]: http://example.com/  "Optional Title Here"


## Biological Submodel

### Functional Groups

#### Ocean Quahog (*Arctica islandica*)

Information on ocean quahog comes from Thorarinsdottir & Einarsson (1996)[^1], Thorarinsdottir & Jacobson (2005)[^2], the Icelandic Ministry of Fisheries[^3], and NOAA[^4].The ocean quahog is a long-lived (up to 400 years) bivalve occupying depths of 4 meters down to approximately 400 meters with depths up to 256 meters reported for Iceland (Thorarinsdottir & Einarsson, 1996). The majority of the catches in Iceland are reported between 5 and 50 meters (Thorarinsdottir & Jacobson, 2005).  They inhabit soft, sandy soil where they are endobenthic and filter-feed on phytoplankton and detritus. They typically avoid gravel areas and have very slow recruitment. Males may mature as early as 10 years (49 mm shell length) and females may mature as early as 13 years (44 mm shell length) (citation in Thorarinsdottir & Jacobson, 2005). Spawning in Iceland peaks between June - July but occurs all year round (citation in Thorarinsdottir & Jacobson, 2005). Larvae are planktonic for 4 - 6 weeks and settlement peaks off Iceland during August. Recruitment has occurred every year since 2002 in Icleand but recruitment appears to peak every 20 years (citation in Thorarinsdottir & Jacobson, 2005). They are a cold-water invertebrate (could be affected by global warming?) and can tolerate temperates up to 12 celsius in Iceland (Thorarinsdottir & Jacobson, 2005).

According to the Icelandic Ministry of Fisheries, the stock size of ocean quahog in Icelandic waters is estimated at over 1 million tons with mean densities of 3.0 (+/- 0.3 SE), 2.8 (+/- 0.6 SE), and 4.2 (+/- 0.5 SE) kg/m$^2$ in the north-west, north, and east regions of Iceland (Thorarinsdottir & Einarsson, 1996).

#### Norway Lobster (*Nephrops norvegicus*)

All information on Norway lobster, *Nephrops norvegicus*, come from Pampoulie et al. (2010)[^5], Eiriksson (1999)[^6], the Icelandic Ministry of Fisheries, and personal communcations with staff at the Marine Research Institute (MRI) in Reykjavík. Norway lobster are found exclusively in the south of Iceland at depths ranging from 100 - 300 meters (Eiriksson, 1999) preferring ocean temperatures of 6 - 9 celsius. They prefer soft bottom substrates (such as clay or sand), are endobenthic and feed on small benthic animals. They typically do not range more than 100 meters and there does not appear to be different genetic populations (Pampoulie et al. 2010). 

Initial biomass estimates and spatial distribution comes from Jónas Jónasson at the MRI. Biomass in 1968 was used for Norway lobster, which was roughly the virgin biomass. This value was 30,940 tons and was distributed to **Atlantis** boxes proportionate to their reported landings. 

#### Iceland Scallop (*Chlamys islandica*)

The initial biomass for Iceland scallop was distributed to the Brieðafjorður region of the **Atlantis** model (i.e. box 31). This biomass was 100,000 tons. 

## Equations
### Conversions
##### Tons to Nitrogen (invertebrates) 

The following `R` function was used to convert tons to either mg N/m$$$^3$$$ (invertebrates occurring in the water column) or mg N/m$$$^2$$$ (epibenthic and endobenthic invertebrates).

    tons_mgN <- function(tons, unit){
    mgN <- (tons * 1e9) / 20 / 5.7 / unit
    return(mgN)
    }

Where *unit* is either the area of the box or the volume of a layer. 

### Rate of Change Equations

#### Primary Producers

Primary producers are modelled as biomass pools (mg N/m$$$^3$$$) in each spatial box. 

Rate of change for water column _w_ for primary producer _PP_ is: 

$$\frac{d(PP_w)}{dt} =  G\_{PP_w} - M\_{lys,PP} - M\_{lin} - M\_{quad} - \sum\_{j = predators}P\_{PP,j}  $$

$$ G\_{PX} = \mu_{PP} * \delta\_{irr} * \delta\_{nut} * \delta\_{space} * PX$$

$$$G\_{PP}$$$ is the growth rate for _PP_; $$$M\_{lys, PP}$$$ is the loss of _PP_ due to lysis; $$$M\_{lin}$$$ and $$$M\_{quad}$$$ are loss due to linear (density-independent) and quadratic (density-dependent) mortality not treated in the model,  respectively; $$$P\_{PP, i}$$$ are the losses of _PP_ to predation by species _i_; $$$\mu\_{PP}$$$ is the maximum growth rate; $$$\delta\_{irr}$$$ is the light limitation; $$$ \delta_{nut} $$$ is nutrient limitation; and $$$ \delta\_{space} $$$ is space limitation.  

#### Invertebrates

Invertebrates are measured either per-area (e.g. epibenthic and endobenthic species) (mg N/m$$$^2$$$) or per-volume (e.g. zooplankton) (mg N/m$$$^3$$$). 
  
Rate of change for a standard invertebrate consumer _SCI_ is:

$$\frac{d(SCI)}{dt} =  G\_{SCI} - M\_{linSCI} - M\_{quadSCI} - \sum\_{j = predator}P\_{SCI, j} - F\_{SCI}$$

$$ G\_{SCI} = \left( \epsilon\_{SCI} * \sum_{i = prey} P\_{i, SCI} + P\_{DL, SCI} * \epsilon\_{SCI, DL} + P\_{DR, SCI} * \epsilon\_{SCI, DR} \right) * \delta\_{space} * \delta\_{O_2}   $$

$$$ G\_{SCI}$$$ is the growth rate for _SCI_; $$$M\_{linSCI}$$$ and $$$M\_{quadSCI}$$$ are again unexplained density-independent and density-dependent mortaility whose purposes are to set a reasonable carrying capacity in __Atlantis__; $$$P\_{SCI, j}$$$ is depredation of _SCI_ by predator _j_; $$$F\_{SCI}$$$ is fishing on the group (if applicable); $$$ \epsilon\_{SCI}$$$ is the growth efficiency of _SCI_ when feeding on living prey; $$$\epsilon\_{SCI, DL}$$$ and $$$\epsilon\_{SCI, DR}$$$ are the efficiencies when eating on labile and refractory detritus; $$$\delta\_{space}$$$ is space limitation; and $$$\delta\_{O_2}$$$ is oxygen limitation.

#### Vertebrates

Vertebrates are tracked in age-groups in **Atlantis** by abundance, structural weight (bones and hard tissue, mg N/m$$$^3$$$), and reserve weight (soft tissue, mg N/m$$$^3$$$). 

Rate of change for a vertebrate group _V_ is:

$$\frac{d(V\_{i,s})}{dt} = G\_{V\_{i, s}} $$

$$\frac{d(V\_{i,r})}{dt} = G\_{V\_{i, r}} $$

$$\frac{d(V\_{i,d})}{dt} = T\_{im,V\_{i}} - T\_{em,V\_{i}} - M\_{linV, i} - M\_{quadV,i} - \sum\_{j = predator}P\_{V, j} - F\_{V_i}$$

Subscript _i_ refers to age group _i_; _s_ for structural weight; *i* for reserve weight; *d* for density; $$$T\_{im,V\_{i}}$$$ and $$$T\_{em,V\_{i}}$$$ represent movement of vertebrates into and out of the polygon. Growth for vertebrates is calculated the same way as for invertebrates with the exception that it is done for each age group and not as biomass pools.

#### Nitrogen

Water column ammonia and nitrate concentrations are governed by uptake by autotrophs, excretion by consumers, nitrification, and denitification.

Rates of change for ammonia and nitrate are:

$$\frac{d(NH\_3)}{dt} = - \sum_{}  $$

**SHOULD FINISH THIS LATER BUT THE CRUMMY NOTATION KILLS ME :(**

## References
[^7]: Anonymous. (2004). Development of structurally detailed statistically testable models of marine populations. Technical report. Fjölrit nr. 119. Marine Research Institute, Reyjavík, Iceland.

[^3]: Anonymous. 2014, March. Icelandic Ministry of Fisheries and Agriculture. url: [http://www.fisheries.is/](http://www.fisheries.is/).

[^4]: Anonymous. 2014, September. NOAA Fishwatch. url: [http://www.fishwatch.gov/seafood_profiles/species/clams/species_pages/ocean_quahog_clam.htm](http://www.fishwatch.gov/seafood_profiles/species/clams/species_pages/ocean_quahog_clam.htm).

[^6]: Eiriksson, H. (1999). Spatial variabilities of CPUE and mean size as possible criteria for unit stock demarcations in analytical assessments of Nephrops at Iceland. *Rit Fiskideild*, 16, 239-246.

[^8]: Link, J. S., Fulton, E. A., & Gamble, R. J. (2010). The northeast US application of ATLANTIS: a full system model exploring marine ecosystem dynamics in a living marine resource management context. *Progress in Oceanography*, 87(1), 214-234.

[^5]: Pampoulie, C., Skirnisdottir, S., Hauksdottir, S., Olafsson, K., Eiríksson, H., Chosson, V., ... & Hjorleifsdottir, S. (2010). A pilot genetic study reveals the absence of spatial genetic structure in Norway lobster (*Nephrops norvegicus*) on fishing grounds in Icelandic waters. *ICES Journal of Marine Science*, 68(1), 20 - 25.

[^9]: Savina, M., Fulton, E., Condie, S., Forrest, R. E., Scandol, J. P., & Astles, K. (2008). Ecologically sustainable development of the regional marine and estuarine resources of NSW: modelling of the NSW continental shelf ecosystem.

[^1]: Thorarinsdottir, G. G, & Einarsson, S.T. (1996). Distribution, abundance, population structure and meat yield of the ocean quahog *Arctica islandica*, in Icelandic waters. *Journal of the Marine Biological Association of the United Kingdom*, 76(04), 1107 – 1114.

[^2]: Thorarinsdóttir, G. G., & Jacobson, L. D. (2005). Fishery biology and biological reference points for management of ocean quahogs (*Arctica islandica*) off Iceland. *Fisheries Research*, 75(1), 97-106.



