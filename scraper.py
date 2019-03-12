#!/usr/bin/env python3

"""Scrape the World Rowing athlete database
---

Scrape the athlete database on the World Rowing website
(http://www.worldrowing.com/athletes/) and extract the following data
for each rower:
- date of birth
- nationality
- gender
- year of first international event
- year of last international event

Usage:
python3 scraper.py [-h|--help]

"""

import re
import requests
import types

from bs4 import BeautifulSoup


def help():
	print(__doc__)
	sys.exit(0)


def get_info(rower_id):
	rower_page = load_page(rower_id)
	if rower_page is None:
		return
	else:
		soup = BeautifulSoup(rower_page, features='html.parser')
		year_regex = re.compile('(\d{4})')

		# Start by checking if the person is in fact an athlete (there
		# are also pages for referees and other officials).
		person_type = soup.find('h2', attrs={'itemprop': 'role'})
		if person_type is None:
			return
		else:
			person_type = person_type.text.strip().lower()
			if 'athlete' not in person_type:
				return
		rower_name = soup.find('span', attrs={'itemprop': 'name'})
		if rower_name is not None:
			rower_name = rower_name.text.strip()
		country = soup.find('span', attrs={'itemprop': 'location'})
		if country is not None:
			country = country.text.strip()
		gender = soup.find('div', attrs={'itemprop': 'gender'})
		if gender is not None:
			gender = gender.text.strip()
		birthdate = soup.find(text='Birthdate')
		birthyear = None
		if birthdate is not None:
			birthdate = birthdate.find_next('div', attrs={'class': 'dd'})
			if birthdate is not None:
				birthdate = birthdate.text.strip()
				birthyear_search = year_regex.search(birthdate)
				if birthyear_search is not None:
					birthyear = birthyear_search.group(1)
		competitions = soup.find_all('h3', attrs={'class': 'table-caption-title'})
		competition_years = []
		if competitions is not None:
			for competition in competitions:
				competition = competition.text.strip()
				year_search = year_regex.search(competition)
				if year_search is not None:
					year = year_search.group(1)
				else:
					year = None
				competition_years.append(year)
			competition_years = [x for x in competition_years if x is not None]
			#if len(competition_years) > 0:
			#	first_competition = min(competition_years)
			#	last_competition = max(competition_years)
		results = soup.find_all('div', attrs={'class': 'recentResultsTableWrap'})
		results_list = []
		if results is not None:
			for result in results:
				# The final result of a competition always comes first,
				# so we can simply use find() instead of find_all().
				race = result.find('td', attrs={'headers': 't1-Race'})
				if race is not None:
					race = race.text.strip()
				position = result.find('td', attrs={'headers': 't1-Position'})
				if position is not None:
					position = position.text.strip()
				category = result.find('td', attrs={'headers': 't1-class'})
				if category is not None:
					category = category.text.strip()
				results_list.append(','.join([race, position, category]))
			results_list = [x for x in results_list if x is not None]
		return {'name': rower_name,
			'country': country,
			'gender': gender,
			'birthdate': birthdate,
			'birthyear': birthyear,
			'competition_years': competition_years,
			'results': results_list
		}


def load_page(rower_id):
	url = 'http://www.worldrowing.com/athletes/athlete/{0}/results/'.format(rower_id)
	try:
		page = requests.get(url)
		return page.content
	except Exception as e:
		return None


def main():
	print('Looking for athletes...')
	out = open('rowers_results_test.txt', 'w')
	out.write('name\tcountry\tgender\tbirthdate\tbirthyear\tcompetition_years\tfirst\tlast\tresults\n')
	for i in range(20001, 20010):
		if i % 2500 == 0:
			print('{0}...'.format(i))
		rower_info = get_info(i)
		if rower_info is not None:
			info_string = ''
			for key, value in rower_info.items():
				if key != 'competition_years' and key != 'results':
					info_string += '{0}\t'.format(value)
				elif key == 'competition_years':
					if len(value) > 0:
						info_string += ','.join(value)
						first_competition = min(value)
						last_competition = max(value)
						info_string += '\t{0}\t{1}'.format(first_competition, last_competition)
					else:
						info_string += 'None\tNone\tNone'
				elif key == 'results':
					if len(value) > 0:
						info_string += '\t{0}'.format(',,'.join(value))
					else:
						info_string += 'None'
			info_string += '\n'
			out.write(info_string)
	out.close()
	print('Done!')

if __name__ == '__main__':
	main()
