FROM mcr.microsoft.com/dotnet/core/sdk:3.1 AS build-dotnet
WORKDIR /app
# Copy csproj and restore as distinct layers
COPY *.sln ./
COPY src/Grinder/Grinder.fsproj ./src/Grinder/Grinder.fsproj
COPY src/Grinder.Common/Grinder.Common.fsproj ./src/Grinder.Common/Grinder.Common.fsproj
COPY src/Grinder.DataAccess/Grinder.DataAccess.csproj ./src/Grinder.DataAccess/Grinder.DataAccess.csproj
COPY tests/Grinder.Tests/Grinder.Tests.fsproj ./tests/Grinder.Tests/Grinder.Tests.fsproj
COPY src/Grinder.ExportTool/Grinder.ExportTool.fsproj ./src/Grinder.ExportTool/Grinder.ExportTool.fsproj
RUN dotnet restore
# Then build app
COPY src/Grinder/. ./src/Grinder
COPY src/Grinder.Common/. ./src/Grinder.Common
COPY src/Grinder.DataAccess/. ./src/Grinder.DataAccess
WORKDIR /app/src/Grinder
RUN dotnet publish -r linux-x64 -c Release -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/core/runtime:3.1

EXPOSE 80

ARG APP_CONFIG
ENV DOTNETRU_APP_CONFIG ${APP_CONFIG}

WORKDIR /app
COPY --from=build-dotnet /app/src/Grinder/out .

RUN mkdir -p /etc/grinder && mkdir -p /app/data

VOLUME /etc/grinder/
VOLUME /app/data/

ENTRYPOINT ["dotnet", "Grinder.dll"]
