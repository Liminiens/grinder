FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build-dotnet
WORKDIR /app
# Copy csproj and restore as distinct layers
COPY *.sln ./
COPY src/Grinder/Grinder.fsproj ./src/Grinder/Grinder.fsproj
COPY src/Grinder.Common/Grinder.Common.fsproj ./src/Grinder.Common/Grinder.Common.fsproj
COPY src/Grinder.DataAccess/Grinder.DataAccess.csproj ./src/Grinder.DataAccess/Grinder.DataAccess.csproj
COPY tests/Grinder.Tests/Grinder.Tests.fsproj ./tests/Grinder.Tests/Grinder.Tests.fsproj
COPY src/Grinder.ExportTool/Grinder.ExportTool.fsproj ./src/Grinder.ExportTool/Grinder.ExportTool.fsproj
COPY src/Grinder.Farmer/Grinder.Farmer.fsproj ./src/Grinder.Farmer/Grinder.Farmer.fsproj
COPY Directory.Build.props ./Directory.Build.props
RUN dotnet tool restore
RUN dotnet restore -r linux-x64
# Then build app
COPY src/Grinder/. ./src/Grinder
COPY src/Grinder.Common/. ./src/Grinder.Common
COPY src/Grinder.DataAccess/. ./src/Grinder.DataAccess
WORKDIR /app/src/Grinder
# Build&Test
RUN dotnet test -c Release -r linux-x64 -o out --no-restore --verbosity normal
# Publish
RUN dotnet publish -r linux-x64 -c Release -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/runtime:5.0

EXPOSE 80

WORKDIR /app
COPY --from=build-dotnet /app/src/Grinder/out .

RUN mkdir -p /etc/grinder && mkdir -p /app/data

VOLUME /etc/grinder/
VOLUME /app/data/

ENTRYPOINT ["dotnet", "Grinder.dll"]
