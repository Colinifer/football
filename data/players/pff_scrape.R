# PFF keys
# https://www.pff.com/api/players/5598/stats?season=2017&week_group=REG

base <- 'https://www.pff.com/api/players'
mid <- 'stats?season'
tail <- 'week_group=REG'

pff_id <- 5598
url <- glue('{base}/{pff_id}/{mid}={year}&{tail}')

# Cookies
merlin_key <- 'SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYNUtNVlRfNEtXTDhZT0ZPZUVXUHBXSmhhbQAAABZndWFyZGlhbl9kZWZhdWx0X3Rva2VubQAAAkpleUpoYkdjaU9pSklVelV4TWlJc0luUjVjQ0k2SWtwWFZDSjkuZXlKaGRXUWlPaUpOWlhKc2FXNGlMQ0psZUhBaU9qRTJNRGt6TnpBMU5qa3NJbWxoZENJNk1UWXdPVE0yTmprMk9Td2lhWE56SWpvaVRXVnliR2x1SWl3aWFuUnBJam9pTnpVek1USTVaVFV0WkRrMlpTMDBOR1ZpTFRsbE56a3RPVEZsWW1Jek9ETXhNek0zSWl3aWJtSm1Jam94TmpBNU16WTJPVFk0TENKd1pXMGlPbnNpWldSblpTSTZNWDBzSW5OMVlpSTZJbnRjSW1WdFlXbHNYQ0k2WENKaGJHVjRkMlZzYzJoQWIzQjBiMjVzYVc1bExtNWxkRndpTEZ3aVptVmhkSFZ5WlhOY0lqcGJYU3hjSW1acGNuTjBYMjVoYldWY0lqcGNJa0ZzWlhoY0lpeGNJbXhoYzNSZmJtRnRaVndpT2x3aVYyVnNjMmhjSWl4Y0luVnBaRndpT2x3aU0yVmtOR05pWkdVdE5HVmtZUzAwWW1ZMExXRTFNVGt0WkdKaVpUTmxZV0ZtWm1aa1hDSXNYQ0oyWlhKMGFXTmhiRndpT2x3aVEyOXVjM1Z0WlhKY0luMGlMQ0owZVhBaU9pSmhZMk5sYzNNaWZRLjg4MUZqWWNQSjZYakhxX3pHTVZQcmlhR3Y3MmZwZDE5dmtQWFJiZkM1bHpHNlZPRWhybm5FSW5ERDRoREt0T0ItVXdsRGVVcGVuUmxBek5FdkZzZGpRbQAAAAlyZXR1cm5fdG9tAAAAAS8.uvhmBAbHgcJAzgbZ5S3G9sq6UcaolnlkQ7DX5308DUQ'
c_groot_refresh_token <- 'P5iYDkHq5fUZ2Rbz60uAePtCFYz0nIIUOoe9Il_oG_lp6dzvoSvq9EmCTvI20bww'
c_groot_access_token <- 'JS01jxvYrNpbw7TI4sVo3AyT-hxdqe7bWK51sC-skqo6spCm-IH8WbeYkNJ5KMf5'

cookies <- c('mailmunch_second_pageview' = 'true',
             '_mailmunch_visitor_id' = '97ebe8d9-2066-4ecd-8448-0555c3ea6d33',
             '_gcl_au' = '1.1.48502121.1606067551',
             '_ga_8Y6RN784SW' = 'GS1.1.1609368889.14.1.1609368985.38',
             '_ga' = 'GA1.2.1698245610.1606067551',
             'seerid' = 'u_440131965741831700',
             '_merlin_key' = merlin_key,
             'c_groot_refresh_token' = c_groot_refresh_token,
             'c_groot_access_token' = c_groot_access_token)

cookie <- paste(names(cookies), cookies, sep = '=', collapse = ';')

pff_player_get = GET (
  url,
  httr::authenticate('alexwelsh@optonline.net', 'GG-A4-J6', type = ),
  config = httr::config(Cookie = cookie),
  content_type_json(),
  user_agent(user_agent),
  httr::add_headers(
    'Host: www.pff.com
    Accept:	text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8
    Accept-Encoding: gzip, deflate, br
    Accept-Language: en-US,en;q=0.5
    Cache-Control: max-age=0
    Connection: keep-alive
    Upgrade-Insecure-Requests: 1
    Cache-Control: max-age=0
    TE: Trailers'
  )
)
pff_player_get$status_code
jsonlite::fromJSON(rawToChar(pff_player_get$content))[[1]]
