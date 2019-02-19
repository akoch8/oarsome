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
			if person_type != 'athlete':
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
		if birthdate is not None:
			birthdate = birthdate.find_next('div', attrs={'class': 'dd'})
			if birthdate is not None:
				birthdate = birthdate.text.strip()
				birthyear_search = year_regex.search(birthdate)
				if birthyear_search is not None:
					birthyear = birthyear_search.group(1)
				else:
					birthyear = None
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
		return {'name': rower_name,
			'country': country,
			'gender': gender,
			'birthdate': birthdate,
			'birthyear': birthyear,
			'competition_years': competition_years
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
	out = open('rowers.txt', 'w')
	out.write('name\tcountry\tgender\tbirthdate\tcompetition_years\tfirst\tlast\n')
	for i in range(1, 48343):
		if i % 5000 == 0:
			print('{0}...'.format(i))
		rower_info = get_info(i)
		if rower_info is not None:
			info_string = ''
			for key, value in rower_info.items():
				if key != 'competition_years':
					info_string += '{0}\t'.format(value)
				else:
					if len(value) > 0:
						info_string += ','.join(value)
						first_competition = min(value)
						last_competition = max(value)
						info_string += '\t{0}\t{1}'.format(first_competition, last_competition)
					else:
						info_string += 'None\tNone\tNone'
			info_string += '\n'
			out.write(info_string)
	out.close()
	print('Done!')

if __name__ == '__main__':
	main()
