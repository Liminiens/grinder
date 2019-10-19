FROM mcr.microsoft.com/dotnet/core/sdk:2.2.4-stretch AS build-dotnet
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
RUN dotnet publish -c Release -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/core/runtime:2.2.4-stretch-slim
WORKDIR /app
COPY --from=build-dotnet /app/src/Grinder/out .

RUN mkdir -p /etc/grinder && mkdir -p /app/data

VOLUME /etc/grinder/
VOLUME /app/data/

ENTRYPOINT ["dotnet", "Grinder.dll"]
