// ERA5-Land orario (T, RH, VPD) + precipitazione giornaliera nella cella di un punto → CSV per R


// ==============
// PARAMETRI
// ==============
var lat = 42.3999;                              // latitudine  (1° numero di Google Maps)
var lon = 12.1987;                              // longitudine (2° numero di Google Maps)
var annoInizio = 1960;                          // primo anno
var annoFine = 2025;                            // ultimo anno (incluso)
var anniPerFile = 10;                           // anni per CSV orario → 1 task di export per blocco
var idOrario = 'ECMWF/ERA5_LAND/HOURLY';        // ERA5-Land orario
var idGiorno = 'ECMWF/ERA5_LAND/DAILY_AGGR';    // ERA5-Land giornaliero
var bandaT  = 'temperature_2m';                 // temperatura a 2 m (K), valore istantaneo
var bandaTd = 'dewpoint_temperature_2m';        // temperatura di rugiada a 2 m (K), valore istantaneo
var bandaP  = 'total_precipitation_sum';        // precipitazione, totale giornaliero (m)
var nomeOrario = 'ERA5Land_orario';             // prefisso dei CSV orari su Drive
var nomeGiorno = 'ERA5Land_prec_giornaliera';   // nome del CSV giornaliero su Drive


// ==============
// 1. PUNTO
// ==============
// GEE vuole [lon, lat] → ordine inverso rispetto a Google Maps
var punto = ee.Geometry.Point([lon, lat]);

// controllo visivo sulla mappa
Map.centerObject(punto, 10);
Map.addLayer(punto, {color: 'red'}, 'punto');


// ==============
// 2. COLLECTION
// ==============
// solo le bande utili (il filtro date è nei blocchi di export)
var collOra    = ee.ImageCollection(idOrario).select([bandaT, bandaTd]);
var collGiorno = ee.ImageCollection(idGiorno).select([bandaP]);

var proj = collOra.first().projection();   // griglia nativa (0.1°)

// controllo: centro della cella che contiene il punto
// → deve coincidere con lon/lat della cella più vicina nel CSV giornaliero
print('Centro cella:', ee.Image.pixelLonLat().reduceRegion({
  reducer: ee.Reducer.first(),
  geometry: punto,
  crs: proj,
  scale: proj.nominalScale()
}));


// ==============
// 3. CONVERSIONE UNITÀ E UMIDITÀ (orario)
// ==============
// T, Td: K → °C
// es(x) = 0.61094 · exp(17.625 · x / (x + 243.04))   pressione di vapore di saturazione (kPa), x in °C (Magnus)
// RH  (%)   = 100 · es(Td) / es(T)
// VPD (kPa) = es(T) − es(Td)
var convertiOra = function(img) {
  var t    = img.select(bandaT).subtract(273.15).rename('t2m');
  var td   = img.select(bandaTd).subtract(273.15).rename('td');
  var esT  = t.multiply(17.625).divide(t.add(243.04)).exp().multiply(0.61094);
  var esTd = td.multiply(17.625).divide(td.add(243.04)).exp().multiply(0.61094);
  var rh   = esTd.divide(esT).multiply(100).min(100).rename('rh');   // max 100%
  var vpd  = esT.subtract(esTd).max(0).rename('vpd');               // min 0
  // addBands su img → l'immagine conserva la sua data
  return img.addBands(ee.Image([t, td, rh, vpd])).select(['t2m', 'td', 'rh', 'vpd']);
};


// ==============
// 4. VALORI NEL PUNTO
// ==============
// valore della cella che contiene il punto, sulla griglia nativa (nessun ricampionamento)
var valoriNelPunto = function(img) {
  return img.reduceRegion({
    reducer: ee.Reducer.first(),   // 1 sola cella → il suo valore
    geometry: punto,
    crs: img.projection(),
    scale: img.projection().nominalScale()
  });
};

// 1 riga per ora; ora in UTC (Italia: +1 in inverno, +2 in estate)
var estraiOra = function(img) {
  return ee.Feature(null, valoriNelPunto(img))   // null = riga senza geometria
    .set('ora_utc', img.date().format('YYYY-MM-dd HH:mm', 'UTC'));
};

// controllo: 24 ore di un giorno d'estate
var prova = collOra
  .filterDate(annoInizio + '-07-01', annoInizio + '-07-02')
  .map(convertiOra)
  .map(estraiOra);
print('Prova, 24 ore:', prova);


// ==============
// 5. EXPORT CSV ORARI → Google Drive (1 file ogni anniPerFile anni)
// ==============
// 1 task per blocco → avviarli tutti dalla scheda Tasks
for (var a = annoInizio; a <= annoFine; a += anniPerFile) {
  var b = Math.min(a + anniPerFile - 1, annoFine);   // ultimo anno del blocco

  var tabOra = collOra
    .filterDate(a + '-01-01', (b + 1) + '-01-01')    // data finale esclusa
    .map(convertiOra)
    .map(estraiOra);

  Export.table.toDrive({
    collection: tabOra,
    description: nomeOrario + '_' + a + '_' + b,
    fileFormat: 'CSV',
    selectors: ['ora_utc', 't2m', 'td', 'rh', 'vpd']   // solo queste colonne, in quest'ordine
  });
}


// ==============
// 6. PRECIPITAZIONE GIORNALIERA → Google Drive (1 file)
// ==============
// m → mm; valori negativi (errori di compressione dei dati) → 0
var convertiGiorno = function(img) {
  var prec = img.select(bandaP).multiply(1000).max(0).rename('prec');
  return img.addBands(prec).select('prec');
};

// 1 riga per giorno (giorno in UTC)
var estraiGiorno = function(img) {
  return ee.Feature(null, valoriNelPunto(img))
    .set('data', img.date().format('YYYY-MM-dd', 'UTC'));
};

var tabGiorno = collGiorno
  .filterDate(annoInizio + '-01-01', (annoFine + 1) + '-01-01')
  .map(convertiGiorno)
  .map(estraiGiorno);

Export.table.toDrive({
  collection: tabGiorno,
  description: nomeGiorno,
  fileFormat: 'CSV',
  selectors: ['data', 'prec']
});
// → avviare i task dalla scheda Tasks (pannello a destra)