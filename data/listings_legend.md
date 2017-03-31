Listing files fields
===

This file contains the list of of variables (listing CSV file fields) and its correspondence with Airbnb website sections

	id				airbnb object id
	listing_url			airbnb url
	scrape_id	
	last_scraped			date format YYYY-MM-DD

## OVERVIEW TAB
	name				Title
	summary				"About this listing" section in Airbnb
	space				"The space" subsection of Description section
	description			same as summary?
	experiences_offered		
	neighborhood_overview		"The neighborhood" subsection of Description section
	notes				"Other things to note" subsection of Description section
	transit				"Getting around" subsection of Description section
	access				"Getting access" subsection of Description section
	interaction			"Interaction with guests" subsection of Description section
	house_rules			"House rules" section
	thumbnail_url			small size image url
	medium_url			medium size image url
	picture_url			large size image url
	xl_picture_url			extra large size image url

## THE HOST TAB (user information)
	host_id				user id
	host_url			user url
	host_name			user name
	host_since			user sign up date
	host_location			user location
	host_about			user description
	host_response_time		user response time
	host_response_rate		user response rate
	host_acceptance_rate		?
	host_is_superhost		t = true / f = false
	host_thumbnail_url		user small photo url
	host_picture_url		user photo url
	host_neighbourhood		user neighbourhood
	host_listings_count		how many listing has this user ### INTERESTING
	host_total_listings_count	don't know the difference between this and the one above
	host_verifications		how the user has been verified
	host_has_profile_pic		t = true / f = false
	host_identity_verified		t = true / f = false

## LISTING LOCATION
	street
	neighbourhood
	neighbourhood_cleansed		?
	neighbourhood_group_cleansed	Provincia in Spain
	city
	state				Comunidad Autonoma in Spain
	zipcode
	market				?
	smart_location			City and country
	country_code
	country
	latitude
	longitude
	is_location_exact		t = true / f = false

## THE SPACE INFO
	property_type			House...
	room_type			Private room...
	accommodates			how many people can be hosted
	bathrooms			number
	bedrooms			number
	beds				number
	bed_type
	amenities			list of amenities
	square_feet

## PRICES SECTION
	price				per night. in dollars
	weekly_price			in dollars
	monthly_price			in dollars
	security_deposit		
	cleaning_fee
	guests_included			how many people allowed
	extra_people			price per night
	minimum_nights
	maximum_nights

## RESERVATION INFO
	calendar_updated		when was updated the availability calendar
	has_availability		?
	availability_30
	availability_60
	availability_90
	availability_365
	calendar_last_scraped		when was scraped the calendar

## REVIEWS TAB
	number_of_reviews		number
	first_review			date. format: YYYY-MM-DD
	last_review			date. format: YYYY-MM-DD
	review_scores_rating		number. 0-10
	review_scores_accuracy		number. 0-10
	review_scores_cleanliness	number. 0-10
	review_scores_checkin		number. 0-10
	review_scores_communication	number. 0-10
	review_scores_location		number. 0-10
	review_scores_value		number. 0-10

## LEGAL ISSUES
	requires_license		t = true / f = false
	license				type of license
	jurisdiction_names


	instant_bookable		t = true / f = false
	cancellation_policy		4 types
	require_guest_profile_picture	t = true / f = false
	require_guest_phone_verificationt = true / f = false
	calculated_host_listings_count	user's listings
	reviews_per_month		average
