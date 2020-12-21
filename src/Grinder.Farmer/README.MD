#Description
This project will deploy Grinder bot to Azure
There are two scenarios
- Deploy Azure resources together with docker image
- Build and push docker image only into predeployed container registry

To deploy everything pass `--all` argument

To deploy just an image don't pass any argument

#Prerequisites
- AZ CLI
- Docker
- Net5 SDK

- Azure credentials (put them in env variables)
  - `VAHTER_DEPLOY_APPID`
  - `VAHTER_DEPLOY_PWD`
  - `VAHTER_DEPLOY_TENANT`
    
- Bot settings (pick one)
  - `settings.json` file
  - `VAHTER_CONFIG` variable
    
#Run
`dotnet run [args]` (from project folder)

or just run from IDE
