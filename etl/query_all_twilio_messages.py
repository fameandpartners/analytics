from twilio.rest import Client
import csv
import json

account_sid = ""
auth_token  = ""

client = Client(account_sid, auth_token)

messages = []

for service in client.ip_messaging.services.list():
    for channel in service.channels.list():
        for message in channel.messages.list():
            parsed_body = json.loads(message.body)
            message_dict = {
                "service_sid": message.service_sid,
                "account_sid": message.account_sid,
                "channel_sid": message.channel_sid,
                "sid": message.sid,
                "date_created": message.date_created,
                "author": parsed_body.get("author"),
                "user_id": parsed_body.get("user_id"),
                "message_type": parsed_body.get("type"),
                "content": str(json.dumps(parsed_body.get("content")))
            }
            messages.append(message_dict)

keys = messages[0].keys()

with open('messages_data.csv', 'wb') as output_file:
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    for message in messages:
        try:
            dict_writer.writerow(message)
        except:
            pass