# Prerequisites

Install AWS `sam` and setup other dependencies:
https://docs.aws.amazon.com/serverless-application-model/latest/developerguide/serverless-sam-cli-install-linux.html


# Deployment

    ./redeploy.sh

# Testing

    # Get the endpoint URL
    aws cloudformation describe-stacks --stack-name aws-sam-getting-started --region us-east-2 --query "Stacks[].Outputs"

    # To exercise the endpoint:
    curl https://pucofjfcw2.execute-api.us-east-2.amazonaws.com/Prod/hello/

    # To view console output after exercising the endpoint:
    sam logs --region us-east-2 -n HelloWorldFunction --stack-name aws-sam-getting-started --tail


# Guides

Hello World Lambda: https://docs.aws.amazon.com/serverless-application-model/latest/developerguide/serverless-getting-started-hello-world.html
