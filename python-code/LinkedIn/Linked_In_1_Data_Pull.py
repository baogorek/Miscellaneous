import oauth2 as oauth
import urlparse 


consumer_key           = ''
consumer_secret        = ''
request_token_url      = 'https://api.linkedin.com/uas/oauth/requestToken?scope=r_fullprofile+r_network+rw_nus'
access_token_url       = 'https://api.linkedin.com/uas/oauth/accessToken'
authorize_url          = 'https://www.linkedin.com/uas/oauth/authenticate'
 
consumer = oauth.Consumer(consumer_key, consumer_secret)

client = oauth.Client(consumer)

resp, content = client.request(request_token_url, "POST")
content_parsed = dict(urlparse.parse_qsl(content))

request_token = oauth.Token( key=content_parsed['oauth_token'], secret=content_parsed['oauth_token_secret'])

# Output request URL to console
print "Go to the following link in your browser:"
print "%s?oauth_token=%s" % (authorize_url, content_parsed['oauth_token'])
print

# Set verifier code
oauth_verifier = raw_input('What is the PIN? ')
# Answer before preceding

request_token.set_verifier(oauth_verifier)
client.token = request_token

resp, content = client.request(access_token_url, "POST")
content_parsed = dict(urlparse.parse_qsl(content))

access_token = oauth.Token(key=content_parsed['oauth_token'], secret=content_parsed['oauth_token_secret'])
client.token = access_token

resp, content = client.request("http://api.linkedin.com/v1/people/~:(first-name,last-name,honors,educations,publications,skills,three-current-positions,three-past-positions,num-recommenders,recommendations-received,member-url-resources,suggestions,volunteer,languages,patents,proposal-comments,interests)")
print content
#"http://api.linkedin.com/v1/people/~(first-name,last-name,honors,skills, publications, educations, three-current-positions, three-past-positions, num-recommenders, recommendation-received, member-url-resources, suggestions, volunteer, languages, patents, proposal-comments, interests)")
resp_connections, content_connections = client.request("http://api.linkedin.com/v1/people/~/connections:(first-name,last-name,summary,current-share,num-connections,specialties,positions,industry)")

out_xml = open("/home/user/eclipse_space/Social_Media/Ben_connections.xml", 'wb')
out_xml.write(content_connections)
out_xml.close()

resp, content = client.request("http://api.linkedin.com/v1/people/id=VKoJ89nW5K:(first-name,last-name,summary,current-share,num-connections,specialties,positions,industry)")





