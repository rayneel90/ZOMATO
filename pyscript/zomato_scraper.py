import requests
import json
class Zomato:

    """ The main scraper for Zomato """

    def __init__(self, key, data_format='application/json'):
        """ Initiates the scraper class. Requires the API key (argument key)

        Parameters
        ---------
            key: The API key provided by Zomato. If don't have the key
              already, Generate a key here - https://developers.zomato.com/api
            data_format: The format in which the data is to be returned.
              Default is 'application/json'. Accepted values are
              'application/json' or 'application/xml'.

        Example
        --------
        scraper = Zomato(key='eeb494140f638584b11b6717ba08a777')
        """
        self.__header = {
            "Accept": data_format,
            "user-key": key,
        }
        self.__base_url = 'https://developers.zomato.com/api/v2.1/search?'
        self.restaurants = []
    def get_restaurant_data(self, **kwargs):
        """ Fetches data of first 100 restaurants matching the filters mentioned

        It takes mustiple keyword arguments. Valid arguments are mentioned below:
        Parameters
        --------
            url:
            kwargs:
                entity_id -- list of integers. Must be valid entity id. can be
                  obtained using 'get_entity' method
                entity_type -- str. valid values are 'city
                q --
                start --
                count --
                lat --
                lon --
                radius --
                cuisines --
                establishment_type --
                collection_id --
                category --
                sort --
                order --

        """
        valid_keys=['entity_id', 'entity_type', 'q', 'start', 'count', 'lat',
                    'lon', 'radius', 'cuisines', 'establishment_type',
                    'collection_id', 'category', 'sort', 'order',
                    ]
        url_extension = ''
        for key,val in kwargs.items():
            if key not in valid_keys:
                raise TypeError('"' + key + '" is not a valid keyword '
                                            'argument for this function')
            if type(val) is list:
                val = '%2c'.join(val)
            if type(val) is not str:
                val = str(val)
            url_extension = url_extension + '='.join((key, val)) + '&'
        url_extension = url_extension[:-1]
        dat=json.loads(
                requests.get(
                    self.__base_url+url_extension,
                    headers=self.__header
                ).text
        )
        if not kwargs.get('start'):
            kwargs['start'] = dat['results_start']
        self.restaurants.extend(dat['restaurants'])
        if kwargs.get('count'):
            print('count exists')
            print('start',kwargs['start'],'count:',
                  kwargs['count'],'found:',dat['results_found'])
            if (dat['results_start'] +dat['results_shown'])\
                < min(
                    dat['results_start'] + kwargs.get('count'),
                    dat['results_found'],
                    100
                ):
                kwargs['start'] += 20
                kwargs['count'] -= 20
                if kwargs['start']>80:
                    kwargs['count'] = 100 - kwargs['start']
                self.get_restaurant_data(**kwargs)

        elif kwargs.get('start') < 100:
            print('start',kwargs['start'],'found:',dat['results_found'])
            kwargs['start'] += 20
            if kwargs['start']>80:
                kwargs['count']=min(dat['results_found'],100)-kwargs['start']
            self.get_restaurant_data(**kwargs)



parser=Zomato('eeb494140f638584b11b6717ba08a777')
parser.get_restaurant_data(start=73,count=55)
print(len(parser.restaurants))

