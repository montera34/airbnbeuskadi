<?php
// Geocoding script using Nominatim https://nominatim.openstreetmap.org/
// to get coordinates using City, Country, street name, house number and Postal Code
function ea_geocoding( $country='',$state='',$city='',$street_name='',$house_number='',$postal_code='',$email='' ) {
	// encode search strings
	$country = urlencode(utf8_encode($country));
	$state = urlencode(utf8_encode($state));
	$city = urlencode(utf8_encode($city));
	$street_name = urlencode(utf8_encode($street_name));
	$house_number = urlencode(utf8_encode($house_number));
	$postal_code = urlencode(utf8_encode($postal_code));
	
	// Create a stream
	$opts = [
		"http" => [
		"method" => "GET",
		"header" =>
			"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n".
			"Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\n".
			"Accept-Encoding: gzip, deflate, br\r\n".
			"Accept-Language: n-US,en;q=0.5\r\n".
			"Connection: keep-alive\r\n".
			"Cookie: qos_token=790276\r\n".
			"DNT: 1\r\n".
			"Host: nominatim.openstreetmap.org\r\n".
			"Upgrade-Insecure-Requests: 1\r\n".
			"User-Agent:Mozilla/5.0 (X11; Linux x86_64; rv:61.0) Gecko/20100101 Firefox/61.0\r\n"

		]
	];
	$context = stream_context_create($opts);

	// use nominatim geocoding service to get coords
	$results_json = file_get_contents("https://nominatim.openstreetmap.org/search?format=json&country=".$country."&city=".$city."&state=".$state."&street=".$house_number."%20".$street_name."&postalcode=".$postal_code."&limit=1&email=".$email);
	$results = json_decode($results_json,TRUE); // if second parameter is set to TRUE, the output is ass. array

	if ( !array_key_exists('0',$results) ) {
//		$results_json = file_get_contents("https://nominatim.openstreetmap.org/search?format=json&country=".$country."&city=".$city."&limit=1");
		$results = json_decode($results_json,TRUE);
	}

	if ( array_key_exists('0',$results) )
		return $results[0];
	else return;
}

// get script arguments
$country = $argv[1];
$state = $argv[2];
$city =  $argv[3];
$csv_filename = $argv[4]; // relative path to data filename
$email = $argv[5]; // relative path to data filename
$line_length = "4096"; // max line lengh (increase in case you have longer lines than 1024 characters)
$delimiter = ","; // field delimiter character
$enclosure = '"'; // field enclosure character

// open the data file
$fp = fopen($csv_filename,'r');

// get data and store it in array
if ( $fp !== FALSE ) { // if the file exists and is readable

	$line = -1;
	while ( ($csv = fgetcsv($fp,$line_length,$delimiter,$enclosure)) !== FALSE ) { // begin main loop
		$line++;
		if ( $line == 0 ) continue;
		if ( $line == 3 ) break;
		$street_name = $csv[2];
		sleep (1);
		$gcoding = ea_geocoding($country,$state,$city,$street_name);
		$lat = $gcoding['lat'];
		$lon = $gcoding['lon'];
	}
	fclose($fp);

} else {
	echo "<h2>Error</h2>
	<p>File with contents not found or not accesible.</p>
	<p>Check the path: " .$csv_filename. ". Maybe it has to be absolute...</p>";
} // end if file exist and is readable

?>
